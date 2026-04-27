-module(prop_registrator_replica_statem).

%% Two-replica convergence under arbitrary interleaving of register,
%% unregister, and message delivery. Models swim as an unordered queue per
%% direction; assumes no message loss (that's the limitation we'd cover with
%% anti-entropy, separately).

-include_lib("proper/include/proper.hrl").

-behaviour(proper_statem).

-export([command/1, initial_state/0, next_state/3, postcondition/3,
	 precondition/2]).
-export([prop_two_replica_convergence/0,
	 prop_merge_recovers_from_partition/0]).

-export([apply_local_a/1, apply_local_b/1,
	 deliver_a_to_b/0, deliver_b_to_a/0,
	 drop_a_to_b/0, drop_b_to_a/0,
	 sync_a_pulls_from_b/0, sync_b_pulls_from_a/0,
	 reset/0]).

%%% ===================================================================
%%% Symbolic state
%%%
%%% We track:
%%%   replicas: the two replica states keyed by name (a, b)
%%%   pending: per-direction in-flight envelope queues
%%% ===================================================================

-record(s, {
	  replicas = #{a => undefined, b => undefined},
	  pending  = #{{a,b} => [], {b,a} => []}
	 }).

initial_state() ->
    #s{}.

%%% ===================================================================
%%% Commands
%%% ===================================================================

command(#s{replicas = #{a := undefined}}) ->
    {call, ?MODULE, reset, []};
command(_S) ->
    %% Delivery calls are no-ops when the queue is empty -- we keep the
    %% abstract command set simple and let the concrete handler dedup.
    frequency([
	       {1, {call, ?MODULE, apply_local_a, [op()]}},
	       {1, {call, ?MODULE, apply_local_b, [op()]}},
	       {2, {call, ?MODULE, deliver_a_to_b, []}},
	       {2, {call, ?MODULE, deliver_b_to_a, []}}
	      ]).

precondition(_S, _Call) ->
    true.

%%% ===================================================================
%%% Generators
%%% ===================================================================

service() ->
    ?LET({Id, Port, Name, Proto},
	 {binary(8), range(1, 65535),
	  oneof([<<"redis">>, <<"nginx">>]),
	  oneof([<<"tcp">>, <<"udp">>])},
	 #{id => Id, port => Port, address => {127,0,0,1},
	   service => Name, protocol => Proto}).

op() ->
    oneof([
	   {add, service()},
	   {remove, service()}
	  ]).

%%% ===================================================================
%%% Concrete callbacks (called during proper_statem replay)
%%% ===================================================================

%% These functions operate on a process dictionary slot keyed by ?MODULE so
%% the statem framework can replay symbolic commands against actual replicas.
%% That keeps the model pure-Erlang without a gen_server in the loop.

reset() ->
    A = registrator_replica:new(<<"AAAAAAAAAAAAAAAA">>),
    B = registrator_replica:new(<<"BBBBBBBBBBBBBBBB">>),
    put({?MODULE, replicas}, #{a => A, b => B}),
    put({?MODULE, pending}, #{{a,b} => [], {b,a} => []}),
    ok.

apply_local_a(Op) -> apply_local(a, Op).
apply_local_b(Op) -> apply_local(b, Op).

apply_local(Side, Op) ->
    Replicas = get({?MODULE, replicas}),
    R = maps:get(Side, Replicas),
    {Env, R1} = registrator_replica:apply_local(Op, R),
    put({?MODULE, replicas}, Replicas#{Side := R1}),
    Pending = get({?MODULE, pending}),
    Other = case Side of a -> b; b -> a end,
    Q = maps:get({Side, Other}, Pending),
    put({?MODULE, pending}, Pending#{{Side, Other} := Q ++ [Env]}),
    ok.

deliver_a_to_b() -> deliver(a, b).
deliver_b_to_a() -> deliver(b, a).

deliver(From, To) ->
    Pending = get({?MODULE, pending}),
    case maps:get({From, To}, Pending) of
	[] -> ok;
	[Env | Rest] ->
	    put({?MODULE, pending}, Pending#{{From, To} := Rest}),
	    Replicas = get({?MODULE, replicas}),
	    R = maps:get(To, Replicas),
	    R1 = registrator_replica:apply_remote(Env, R),
	    put({?MODULE, replicas}, Replicas#{To := R1}),
	    ok
    end.

%% Drop the next pending envelope on a directional queue without delivering.
%% Models swim's UDP loss outside the retransmit window.
drop_a_to_b() -> drop(a, b).
drop_b_to_a() -> drop(b, a).

drop(From, To) ->
    Pending = get({?MODULE, pending}),
    case maps:get({From, To}, Pending) of
	[] -> ok;
	[_Env | Rest] ->
	    put({?MODULE, pending}, Pending#{{From, To} := Rest}),
	    ok
    end.

%% TCP state-sync pull: caller takes the peer's full sync_payload and merges
%% it locally. Models the actual anti-entropy mechanism.
sync_a_pulls_from_b() -> sync_pull(a, b).
sync_b_pulls_from_a() -> sync_pull(b, a).

sync_pull(Self, Peer) ->
    Replicas = get({?MODULE, replicas}),
    PeerR = maps:get(Peer, Replicas),
    SelfR = maps:get(Self, Replicas),
    Payload = registrator_replica:sync_payload(PeerR),
    SelfR1 = registrator_replica:merge_sync(Payload, SelfR),
    put({?MODULE, replicas}, Replicas#{Self := SelfR1}),
    ok.

%%% ===================================================================
%%% Symbolic next_state / postcondition
%%% ===================================================================

next_state(_S, _V, {call, _, reset, []}) ->
    #s{replicas = #{a => initialised, b => initialised}};
next_state(S, _V, {call, _, _, _}) ->
    %% Abstract state is only used to gate command generation. We let the
    %% concrete process-dict state drive convergence checking.
    S.

postcondition(_S, _Call, _Res) ->
    true.

%%% ===================================================================
%%% Convergence property
%%%
%%% After the command sequence is replayed, drain all pending queues
%%% (deliver in any order until both empty) and assert both replicas have
%%% the same value().
%%% ===================================================================

prop_two_replica_convergence() ->
    ?FORALL(Cmds, commands(?MODULE),
	    begin
		reset(),
		{_H, _S, Result} = run_commands(?MODULE, Cmds),
		drain_until_quiescent(),
		Replicas = get({?MODULE, replicas}),
		ValueA = lists:sort(registrator_replica:value(maps:get(a, Replicas))),
		ValueB = lists:sort(registrator_replica:value(maps:get(b, Replicas))),
		?WHENFAIL(
		   io:format("Result=~p~nA=~p~nB=~p~n",
			     [Result, ValueA, ValueB]),
		   Result =:= ok andalso ValueA =:= ValueB)
	    end).

drain_until_quiescent() ->
    Pending = get({?MODULE, pending}),
    case maps:fold(fun(_, V, Acc) -> Acc + length(V) end, 0, Pending) of
	0 -> ok;
	_ ->
	    case maps:get({a,b}, Pending) of
		[_|_] -> deliver_a_to_b();
		[] -> ok
	    end,
	    case maps:get({b,a}, get({?MODULE, pending})) of
		[_|_] -> deliver_b_to_a();
		[] -> ok
	    end,
	    drain_until_quiescent()
    end.

%%% ===================================================================
%%% Partition recovery property
%%%
%%% Like prop_two_replica_convergence but with `drop_one` calls that
%%% simulate UDP loss. After draining, do a final bidirectional state
%%% sync (TCP pull) -- both replicas must converge.
%%% ===================================================================

prop_merge_recovers_from_partition() ->
    ?FORALL(Cmds, partition_command_seq(),
	    begin
		reset(),
		[apply_partition_cmd(C) || C <- Cmds],
		drain_until_quiescent(),
		%% Final bidirectional state sync. After this, both replicas
		%% must agree on the visible value regardless of any drops
		%% that happened mid-sequence.
		sync_a_pulls_from_b(),
		sync_b_pulls_from_a(),
		Replicas = get({?MODULE, replicas}),
		ValueA = lists:sort(registrator_replica:value(maps:get(a, Replicas))),
		ValueB = lists:sort(registrator_replica:value(maps:get(b, Replicas))),
		?WHENFAIL(
		   io:format("Cmds=~p~nA=~p~nB=~p~n",
			     [Cmds, ValueA, ValueB]),
		   ValueA =:= ValueB)
	    end).

partition_command_seq() ->
    list(partition_cmd()).

partition_cmd() ->
    frequency([
	       {3, {register, oneof([a, b]), service()}},
	       {1, {unregister, oneof([a, b]), service()}},
	       {3, {deliver, oneof([{a,b}, {b,a}])}},
	       {2, {drop,    oneof([{a,b}, {b,a}])}}
	      ]).

apply_partition_cmd({register, Side, Svc})   -> apply_local(Side, {add, Svc});
apply_partition_cmd({unregister, Side, Svc}) -> apply_local(Side, {remove, Svc});
apply_partition_cmd({deliver, {a,b}})        -> deliver_a_to_b();
apply_partition_cmd({deliver, {b,a}})        -> deliver_b_to_a();
apply_partition_cmd({drop, {a,b}})           -> drop_a_to_b();
apply_partition_cmd({drop, {b,a}})           -> drop_b_to_a().
