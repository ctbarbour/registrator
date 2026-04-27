-module(registrator_nodes).

-behavior(gen_server).

-export([start_link/1, register/1, lookup/0, lookup/1, lookup/2,
	 unregister/1]).
-export([get_sync_payload/0]).
-export([init/1, handle_call/3, handle_info/2, handle_cast/2, code_change/3,
	 terminate/2]).

-define(STARTUP_SYNC_DELAY, 500).
-define(DEFAULT_SYNC_INTERVAL, 300_000).
-define(DEFAULT_STATE_SYNC_PORT_OFFSET, 1000).
-define(SYNC_TIMEOUT, 10_000).
-define(STARTUP_RETRY_DELAY, 30_000).
-define(STARTUP_PULL_FANOUT, 3).

-type seed_host() :: inet:ip_address() | inet:hostname().
-type seed() :: {seed_host(), inet:port_number()}.

-record(group, {
	  name             :: atom(),
	  state_sync_port  :: inet:port_number(),
	  seeds = []       :: [seed()]
	 }).

-record(state, {
	  replica            :: registrator_replica:replica(),
	  gossip_groups = [] :: [#group{}],
	  sync_interval_ms   :: pos_integer(),
	  sync_timer         :: reference() | undefined
	 }).

start_link(Groups) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Groups], []).

register(Service) when is_map(Service) ->
    gen_server:call(?MODULE, {register, Service}).

unregister(Id) ->
    gen_server:call(?MODULE, {unregister, Id}).

lookup() ->
    gen_server:call(?MODULE, lookup).

lookup(Id) ->
    gen_server:call(?MODULE, {lookup, Id}).

lookup(Service, Protocol) ->
    gen_server:call(?MODULE, {lookup, Service, Protocol}).

%% Called by registrator_state_sync_server to fetch the current sync payload.
get_sync_payload() ->
    gen_server:call(?MODULE, get_sync_payload).

init([Groups]) ->
    SyncInterval = application:get_env(registrator, sync_interval_ms,
				       ?DEFAULT_SYNC_INTERVAL),
    GroupRecs = lists:map(fun start_group/1, Groups),
    erlang:send_after(?STARTUP_SYNC_DELAY, self(), startup_sync),
    Ref = erlang:send_after(SyncInterval, self(), sync_tick),
    {ok, #state{replica=registrator_replica:new(),
		gossip_groups=GroupRecs,
		sync_interval_ms=SyncInterval,
		sync_timer=Ref}}.

start_group({Name, Config0}) when is_atom(Name), is_map(Config0) ->
    Config = resolve_swim_config(Config0),
    SwimPort = maps:get(port, Config, 5000),
    SyncPort = maps:get(state_sync_port, Config0,
			SwimPort + ?DEFAULT_STATE_SYNC_PORT_OFFSET),
    ConfigSeeds = normalize_seeds(maps:get(seeds, Config0, [])),
    Seeds = ConfigSeeds ++ env_seeds(),
    SwimConfig = maps:without([state_sync_port, seeds], Config),
    {ok, _Pid} = swim:start(Name, SwimConfig),
    ok = swim:subscribe(Name, user),
    ok = swim:subscribe(Name, membership),
    #group{name=Name, state_sync_port=SyncPort, seeds=Seeds}.

normalize_seeds(Seeds) when is_list(Seeds) ->
    [{normalize_host(H), Port} || {H, Port} <- Seeds].

normalize_host(H) when is_tuple(H)  -> H;
normalize_host(H) when is_binary(H) -> binary_to_list(H);
normalize_host(H) when is_list(H)   -> H.

%% REGISTRATOR_SEEDS=host:port,host:port,...
%% Hosts may be IPv4 literals or DNS names; gen_tcp resolves at connect time.
env_seeds() ->
    case os:getenv("REGISTRATOR_SEEDS") of
	false -> [];
	""    -> [];
	Str   -> parse_env_seed_list(Str)
    end.

parse_env_seed_list(Str) ->
    Tokens = [T || T <- string:tokens(Str, ", "), T =/= ""],
    lists:foldr(fun parse_env_seed/2, [], Tokens).

parse_env_seed(Tok, Acc) ->
    case string:tokens(Tok, ":") of
	[Host, PortStr] ->
	    try [{Host, list_to_integer(PortStr)} | Acc]
	    catch error:badarg ->
		    error_logger:warning_msg("ignoring REGISTRATOR_SEEDS entry ~p: bad port", [Tok]),
		    Acc
	    end;
	_ ->
	    error_logger:warning_msg("ignoring REGISTRATOR_SEEDS entry ~p: expected host:port", [Tok]),
	    Acc
    end.

%% Translate registrator's group config into swim's config.
%%
%% Supported knobs:
%%   advertise_ip :: inet:ip4_address() | auto    -- gossiped to peers
%%   bind_ip      :: inet:ip4_address()           -- what swim binds locally
%%
%% Today swim only takes a single `ip` field, so we collapse: advertise_ip
%% wins; bind_ip is reserved for when swim grows a separate bind option.
%% Either field defaulting to `auto` (or unset) triggers IPv4 auto-detection.
resolve_swim_config(Config) ->
    Advertise = resolve_advertise(maps:get(advertise_ip, Config, auto)),
    Stripped = maps:without([advertise_ip, bind_ip], Config),
    Stripped#{ip => Advertise}.

resolve_advertise(auto) ->
    case first_routable_ipv4() of
	{ok, Ip} -> Ip;
	error    -> {127,0,0,1}
    end;
resolve_advertise(Ip) when is_tuple(Ip) ->
    Ip.

first_routable_ipv4() ->
    case inet:getifaddrs() of
	{ok, IfList} -> scan_ifaces(IfList);
	_            -> error
    end.

scan_ifaces([]) ->
    error;
scan_ifaces([{_Iface, Opts} | Rest]) ->
    Routable = [Addr || {addr, {_,_,_,_} = Addr} <- Opts,
			Addr =/= {127,0,0,1},
			Addr =/= {0,0,0,0}],
    case Routable of
	[Ip | _] -> {ok, Ip};
	[]       -> scan_ifaces(Rest)
    end.

handle_call(lookup, _From, #state{replica=R} = State) ->
    {reply, registrator_replica:value(R), State};
handle_call({lookup, Id}, _From, #state{replica=R} = State) ->
    {reply, registrator_replica:lookup_by_id(Id, R), State};
handle_call({lookup, Service, Protocol}, _From, #state{replica=R} = State) ->
    Pred = fun(#{service := S, protocol := P}) ->
		   S =:= Service andalso P =:= Protocol
	   end,
    {reply, registrator_replica:lookup(Pred, R), State};
handle_call({register, Service}, _From, State) ->
    {reply, ok, do_local({add, Service}, State)};
handle_call({unregister, Id}, _From, #state{replica=R} = State) ->
    Matches = registrator_replica:lookup_by_id(Id, R),
    State1 = lists:foldl(fun(M, S) -> do_local({remove, M}, S) end,
			 State, Matches),
    {reply, ok, State1};
handle_call(get_sync_payload, _From, #state{replica=R} = State) ->
    {reply, registrator_replica:sync_payload(R), State}.

handle_cast({merge_remote_sync, Payload}, #state{replica=R} = State) ->
    R1 = registrator_replica:merge_sync(Payload, R),
    {noreply, State#state{replica=R1}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(startup_sync, #state{gossip_groups=Groups} = State) ->
    Server = self(),
    [spawn(fun() -> startup_sync_group(Server, G) end) || G <- Groups],
    {noreply, State};
handle_info({startup_sync_failed, Name}, State) ->
    error_logger:warning_msg("startup sync for group ~p found no reachable seed; "
			     "retrying in ~pms", [Name, ?STARTUP_RETRY_DELAY]),
    erlang:send_after(?STARTUP_RETRY_DELAY, self(), startup_sync),
    {noreply, State};
handle_info(sync_tick, State) ->
    State1 = run_sync_tick(State),
    Ref = erlang:send_after(State1#state.sync_interval_ms, self(), sync_tick),
    {noreply, State1#state{sync_timer=Ref}};
handle_info({swim, {user, Bin}}, State) when is_binary(Bin) ->
    handle_user_event(safe_term(Bin), State);
handle_info({swim, {membership, Event}}, State) ->
    handle_membership_event(Event, State);
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%% ===================================================================
%%% Op pump
%%% ===================================================================

do_local(Op, #state{replica=R, gossip_groups=Groups} = State) ->
    {Envelope, R1} = registrator_replica:apply_local(Op, R),
    Payload = term_to_binary(Envelope),
    _ = [swim:publish(G#group.name, Payload) || G <- Groups],
    State#state{replica=R1}.

handle_user_event({op, _Actor, _Seq, _Op} = Envelope,
		  #state{replica=R} = State) ->
    R1 = registrator_replica:apply_remote(Envelope, R),
    {noreply, State#state{replica=R1}};
handle_user_event(Other, State) ->
    ok = error_logger:info_msg("Received unmatched user event: ~p", [Other]),
    {noreply, State}.

handle_membership_event({alive, Member}, State) ->
    ok = error_logger:info_msg("Member alive ~p", [Member]),
    {noreply, State};
handle_membership_event({faulty, Member, _From}, State) ->
    ok = error_logger:info_msg("Member faulty ~p", [Member]),
    {noreply, State};
handle_membership_event(_Other, State) ->
    {noreply, State}.

%%% ===================================================================
%%% Anti-entropy: pick a random peer per group, pull their state, merge.
%%% ===================================================================

run_sync_tick(#state{gossip_groups=Groups} = State) ->
    lists:foreach(fun sync_one_group/1, Groups),
    State.

sync_one_group(#group{name=Name, state_sync_port=Port}) ->
    case pick_peer(Name) of
	none ->
	    ok;
	{IpAddr, _MemberPort} ->
	    case registrator_state_sync_client:pull(IpAddr, Port, ?SYNC_TIMEOUT) of
		{ok, Payload} ->
		    gen_server:cast(?MODULE, {merge_remote_sync, Payload});
		{error, Reason} ->
		    error_logger:info_msg("state sync to ~p:~p failed: ~p",
					  [IpAddr, Port, Reason])
	    end
    end.

pick_peer(Name) ->
    Self = swim:myself(Name),
    Members = [M || M <- swim:members(Name), M =/= Self],
    case Members of
	[] -> none;
	_  -> lists:nth(rand:uniform(length(Members)), Members)
    end.

safe_term(Bin) ->
    try binary_to_term(Bin, [safe])
    catch error:badarg -> {error, bad_term}
    end.

%%% ===================================================================
%%% Startup: join swim seeds, then pull state from up to N seeds in
%%% parallel and merge each response. Runs in a spawned worker so the
%%% gen_server stays responsive while seeds are unreachable.
%%% ===================================================================

startup_sync_group(_Server, #group{seeds=[]}) ->
    ok;
startup_sync_group(Server, #group{name=Name, seeds=Seeds}) ->
    Shuffled = shuffle(Seeds),
    case join_any(Name, Shuffled) of
	ok ->
	    Targets = lists:sublist(Shuffled, ?STARTUP_PULL_FANOUT),
	    parallel_pull_and_merge(Server, Targets);
	error ->
	    Server ! {startup_sync_failed, Name}
    end.

join_any(_Name, []) ->
    error;
join_any(Name, [{Ip, Port} | Rest]) ->
    case swim:join(Name, {Ip, Port}) of
	ok -> ok;
	_  -> join_any(Name, Rest)
    end.

parallel_pull_and_merge(Server, Targets) ->
    Self = self(),
    Refs = [begin
		Ref = make_ref(),
		spawn(fun() ->
			      Result = registrator_state_sync_client:pull(
					 Ip,
					 Port + ?DEFAULT_STATE_SYNC_PORT_OFFSET,
					 ?SYNC_TIMEOUT),
			      Self ! {Ref, Result}
		      end),
		Ref
	    end || {Ip, Port} <- Targets],
    [collect_pull(Server, Ref) || Ref <- Refs],
    ok.

collect_pull(Server, Ref) ->
    receive
	{Ref, {ok, Payload}} ->
	    gen_server:cast(Server, {merge_remote_sync, Payload});
	{Ref, {error, _Reason}} ->
	    ok
    after ?SYNC_TIMEOUT + 1000 ->
	    ok
    end.

shuffle(Xs) ->
    [X || {_, X} <- lists:sort([{rand:uniform(), X} || X <- Xs])].
