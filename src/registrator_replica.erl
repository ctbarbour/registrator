-module(registrator_replica).

%% Pure op-based ORSet replica. Side-effect free so it can be driven from
%% proper_statem without spinning up swim or gen_servers.

-export([new/0, new/1]).
-export([apply_local/2, apply_remote/2]).
-export([sync_payload/1, merge_sync/2]).
-export([value/1, lookup/2, lookup_by_id/2]).
-export([actor/1, local_seq/1, seen/1]).

-record(replica, {
	  actor      :: binary(),
	  local_seq  = 0 :: non_neg_integer(),
	  seen       = #{} :: #{binary() => non_neg_integer()},
	  nodes      :: term()
	 }).

-opaque replica()      :: #replica{}.
-type op()             :: {add, term()} | {remove, term()}.
-type envelope()       :: {op, binary(), pos_integer(), op()}.
-type sync_payload()   :: {sync_v1, term(), #{binary() => non_neg_integer()}}.

-export_type([replica/0, op/0, envelope/0, sync_payload/0]).

-spec new() -> replica().
new() ->
    new(crypto:strong_rand_bytes(16)).

-spec new(binary()) -> replica().
new(Actor) when is_binary(Actor) ->
    #replica{actor=Actor, nodes=riak_dt_orswot:new()}.

%% Apply an op locally. Returns the envelope to gossip plus the updated replica.
-spec apply_local(op(), replica()) -> {envelope(), replica()}.
apply_local(Op, #replica{actor=A, local_seq=Seq, seen=Seen, nodes=Nodes} = R) ->
    NewSeq = Seq + 1,
    NewNodes = apply_op_to_set(Op, A, Nodes),
    Envelope = {op, A, NewSeq, Op},
    {Envelope, R#replica{local_seq=NewSeq,
			 seen=Seen#{A => NewSeq},
			 nodes=NewNodes}}.

%% Apply a remote envelope. Dedupes by (Actor, Seq); ignores our own bounce-back.
-spec apply_remote(envelope(), replica()) -> replica().
apply_remote({op, Actor, _Seq, _Op}, #replica{actor=Actor} = R) ->
    R;
apply_remote({op, Actor, Seq, Op}, #replica{seen=Seen, nodes=Nodes} = R) ->
    Last = maps:get(Actor, Seen, 0),
    if
	Seq =< Last ->
	    R;
	true ->
	    NewNodes = apply_op_to_set(Op, Actor, Nodes),
	    R#replica{seen=Seen#{Actor => Seq}, nodes=NewNodes}
    end.

%% Build a payload for TCP anti-entropy. Carries the full ORSet plus the seen
%% map so the receiver can converge on both visible state and dedup table.
-spec sync_payload(replica()) -> sync_payload().
sync_payload(#replica{nodes=Nodes, seen=Seen}) ->
    {sync_v1, Nodes, Seen}.

-spec merge_sync(sync_payload(), replica()) -> replica().
merge_sync({sync_v1, RemoteNodes, RemoteSeen},
	   #replica{nodes=Nodes, seen=Seen} = R) ->
    NewNodes = riak_dt_orswot:merge(Nodes, RemoteNodes),
    NewSeen = elementwise_max(Seen, RemoteSeen),
    R#replica{nodes=NewNodes, seen=NewSeen}.

-spec value(replica()) -> [term()].
value(#replica{nodes=Nodes}) ->
    riak_dt_orswot:value(Nodes).

-spec lookup(fun((term()) -> boolean()), replica()) -> [term()].
lookup(Pred, #replica{nodes=Nodes}) ->
    lists:filter(Pred, riak_dt_orswot:value(Nodes)).

-spec lookup_by_id(term(), replica()) -> [term()].
lookup_by_id(Id, R) ->
    lookup(fun(#{id := I}) -> I =:= Id end, R).

-spec actor(replica()) -> binary().
actor(#replica{actor=A}) -> A.

-spec local_seq(replica()) -> non_neg_integer().
local_seq(#replica{local_seq=S}) -> S.

-spec seen(replica()) -> #{binary() => non_neg_integer()}.
seen(#replica{seen=M}) -> M.

%%% ===================================================================
%%% Internal
%%% ===================================================================

apply_op_to_set({add, Element}, Actor, Set) ->
    {ok, NewSet} = riak_dt_orswot:update({add, Element}, Actor, Set),
    NewSet;
apply_op_to_set({remove, Element}, Actor, Set) ->
    case riak_dt_orswot:update({remove, Element}, Actor, Set) of
	{ok, NewSet} -> NewSet;
	{error, {precondition, {not_present, _}}} -> Set
    end.

elementwise_max(A, B) ->
    Keys = lists:usort(maps:keys(A) ++ maps:keys(B)),
    lists:foldl(
      fun(K, Acc) ->
	      Acc#{K => max(maps:get(K, A, 0), maps:get(K, B, 0))}
      end, #{}, Keys).
