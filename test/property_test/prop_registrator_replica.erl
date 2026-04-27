-module(prop_registrator_replica).

-include_lib("proper/include/proper.hrl").

-export([prop_remote_idempotent/0,
	 prop_local_bounce_is_noop/0,
	 prop_seen_monotonic/0,
	 prop_stale_ops_are_noop/0,
	 prop_add_remove_clears/0,
	 prop_merge_idempotent/0,
	 prop_merge_commutative/0]).

%%% ===================================================================
%%% Generators
%%% ===================================================================

actor() ->
    binary(16).

service() ->
    ?LET({Id, Port, Name, Proto},
	 {binary(8), range(1, 65535),
	  oneof([<<"redis">>, <<"nginx">>, <<"foo">>]),
	  oneof([<<"tcp">>, <<"udp">>])},
	 #{id => Id, port => Port, address => {127,0,0,1},
	   service => Name, protocol => Proto}).

op() ->
    oneof([
	   {add, service()},
	   {remove, service()}
	  ]).

envelope(Actor) ->
    ?LET({Seq, Op}, {pos_integer(), op()},
	 {op, Actor, Seq, Op}).

%% Replica seeded with a few local ops so it has non-empty seen and nodes.
populated_replica() ->
    ?LET(Ops, list(op()),
	 lists:foldl(
	   fun(Op, R) ->
		   {_, R1} = registrator_replica:apply_local(Op, R),
		   R1
	   end, registrator_replica:new(), Ops)).

%%% ===================================================================
%%% Properties
%%% ===================================================================

%% (1) apply_remote of the same envelope twice is idempotent.
prop_remote_idempotent() ->
    ?FORALL({R, Env},
	    {populated_replica(),
	     ?LET(A, actor(), envelope(A))},
	    begin
		Once = registrator_replica:apply_remote(Env, R),
		Twice = registrator_replica:apply_remote(Env, Once),
		Once =:= Twice
	    end).

%% (2) The envelope a replica produces for itself, fed back into apply_remote,
%% is a no-op (bounce-back filter).
prop_local_bounce_is_noop() ->
    ?FORALL({R, Op}, {populated_replica(), op()},
	    begin
		{Env, R1} = registrator_replica:apply_local(Op, R),
		R2 = registrator_replica:apply_remote(Env, R1),
		R1 =:= R2
	    end).

%% (3) seen[Actor] is non-decreasing over any sequence of apply_remote calls.
prop_seen_monotonic() ->
    ?FORALL({R0, A, Envs},
	    {populated_replica(), actor(),
	     non_empty(list(?LET(Seq, pos_integer(),
				 {op, undef, Seq, undef})))},
	    begin
		%% Replace the placeholder actor in each envelope with A.
		Envs1 = [{op, A, S, {add, #{id => <<>>, port => 1,
					     address => {127,0,0,1},
					     service => <<"x">>,
					     protocol => <<"tcp">>}}}
			 || {op, _, S, _} <- Envs],
		{Final, AllOk} =
		    lists:foldl(
		      fun(E, {R, Ok}) ->
			      Before = maps:get(A, registrator_replica:seen(R), 0),
			      R1 = registrator_replica:apply_remote(E, R),
			      After = maps:get(A, registrator_replica:seen(R1), 0),
			      {R1, Ok andalso After >= Before}
		      end, {R0, true}, Envs1),
		_ = Final,
		AllOk
	    end).

%% (4) Stale ops (Seq <= seen[Actor]) leave the replica unchanged.
prop_stale_ops_are_noop() ->
    ?FORALL({R0, A, FreshSeq, StaleDelta, Op},
	    {populated_replica(), actor(),
	     range(2, 100), range(1, 100), op()},
	    ?IMPLIES(StaleDelta < FreshSeq,
		     begin
			 %% Bring seen[A] up to FreshSeq via a fresh op.
			 Fresh = {op, A, FreshSeq, Op},
			 R1 = registrator_replica:apply_remote(Fresh, R0),
			 %% Now feed a stale envelope.
			 StaleSeq = FreshSeq - StaleDelta,
			 Stale = {op, A, StaleSeq, Op},
			 R2 = registrator_replica:apply_remote(Stale, R1),
			 R1 =:= R2
		     end)).

%% (5) Sanity: register-then-unregister-the-same-service yields a value
%% list that doesn't contain that service.
prop_add_remove_clears() ->
    ?FORALL(Svc, service(),
	    begin
		R0 = registrator_replica:new(),
		{_, R1} = registrator_replica:apply_local({add, Svc}, R0),
		{_, R2} = registrator_replica:apply_local({remove, Svc}, R1),
		not lists:member(Svc, registrator_replica:value(R2))
	    end).

%% (6) merge_sync is idempotent: merging the same payload twice equals once.
prop_merge_idempotent() ->
    ?FORALL({R, RemoteSrc},
	    {populated_replica(), populated_replica()},
	    begin
		Payload = registrator_replica:sync_payload(RemoteSrc),
		Once = registrator_replica:merge_sync(Payload, R),
		Twice = registrator_replica:merge_sync(Payload, Once),
		lists:sort(registrator_replica:value(Once))
		    =:= lists:sort(registrator_replica:value(Twice))
	    end).

%% (7) merge_sync is commutative on visible value: applying P1 then P2
%% gives the same observable set as applying P2 then P1, regardless of
%% internal clock differences.
prop_merge_commutative() ->
    ?FORALL({R, Src1, Src2},
	    {populated_replica(), populated_replica(), populated_replica()},
	    begin
		P1 = registrator_replica:sync_payload(Src1),
		P2 = registrator_replica:sync_payload(Src2),
		L = registrator_replica:merge_sync(P2,
		      registrator_replica:merge_sync(P1, R)),
		Rr = registrator_replica:merge_sync(P1,
		      registrator_replica:merge_sync(P2, R)),
		lists:sort(registrator_replica:value(L))
		    =:= lists:sort(registrator_replica:value(Rr))
	    end).
