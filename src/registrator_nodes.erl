-module(registrator_nodes).

-behavior(gen_server).

-export([start_link/2, register/1, lookup/0, lookup/1, lookup/2,
	 unregister/1]).
-export([init/1, handle_call/3, handle_info/2, handle_cast/2, code_change/3,
	 terminate/2]).

-record(state, {
	  nodes = [] :: list(),
	  actor :: term(),
	  gossip_groups = [] :: list()
	 }).

start_link(Actor, Groups) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Actor, Groups], []).

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

init([Actor, Groups]) ->
    Pids = lists:map(
	     fun({Name, Opts}) ->
		     LocalMember = proplists:get_value(local_member, Opts),
		     Keys = proplists:get_value(keys, Opts),
		     {ok, Pid} = swim:start_link(Name, LocalMember, Keys, Opts),
		     Pid
	     end, Groups),
    {ok, #state{nodes=riak_dt_orswot:new(),
		actor=Actor,
		gossip_groups=Pids}}.

handle_call(lookup, _From, State) ->
    #state{nodes=Nodes} = State,
    {reply, riak_dt_orswot:value(Nodes), State};
handle_call({lookup, Id}, _From, State) ->
    #state{nodes=Nodes} = State,
    Matches = lists:filter(
		fun(#{id := I}) ->
			   case I of
			       Id -> true;
			       _ -> false
			   end
		end, riak_dt_orswot:value(Nodes)),
    {reply, Matches, State};
handle_call({lookup, Service, Protocol}, _From, State) ->
    #state{nodes=Nodes} = State,
    Matches = lists:filter(
		fun(E) ->
			#{service := S, protocol := P} = E,
			case {S, P} of
			    {Service, Protocol} -> true;
			    _ -> false
			end
		end, riak_dt_orswot:value(Nodes)),
    {reply, Matches, State};
handle_call({register, Node}, _From, State) ->
    #state{gossip_groups=Groups, actor=Actor} = State,
    NewState = add_node(Actor, Node, State),
    _ = lists:foreach(fun(Group) ->
			      ok = swim:publish(Group, {register, Actor, Node})
		      end, Groups),
    {reply, ok, NewState};
handle_call({unregister, Id}, _From, State) ->
    #state{gossip_groups=Groups, actor=Actor} = State,
    NewState = remove_node(Actor, Id, State),
    _ = lists:foreach(fun(Group) ->
			      ok = swim:publish(Group, {unregister, Actor, Id})
		      end, Groups),
    {reply, ok, NewState}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({swim, {Type, Event}}, State) ->
    handle_swim_event(Type, Event, State);
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

handle_swim_event(user, {register, Actor, Node}, State) ->
    ok = error_logger:info_msg("Received register event: ~p", [Node]),
    NewState = add_node(Actor, Node, State),
    {noreply, NewState};
handle_swim_event(user, {unregister, Actor, Id}, State) ->
    ok = error_logger:info_msg("Receive unregister event: ~p", [Id]),
    NewState = remove_node(Actor, Id, State),
    {noreply, NewState};
handle_swim_event(user, M, State) ->
    ok = error_logger:info_msg("Receieved unmatched user message: ~p", [M]),
    {noreply, State};
handle_swim_event(membership, {alive, Member, _Inc}, State) ->
    #state{gossip_groups=Groups, nodes=Nodes, actor=Actor} = State,
    ok = error_logger:info_msg("Member alive ~p", [Member]),
    lists:foreach(fun(Group) ->
			  [swim:publish(Group, {register, Actor, Service}) || Service <- riak_dt_orswot:value(Nodes)]
		  end, Groups),
    {noreply, State};
handle_swim_event(membership, {faulty, Member, _Inc}, State) ->
    ok = error_logger:info_msg("Member faulty ~p", [Member]),
    {noreply, State}.

add_node(Actor, Node, State) ->
    #state{nodes=Nodes} = State,
    {ok, NewNodes} = riak_dt_orswot:update({add, Node}, Actor, Nodes),
    State#state{nodes=NewNodes}.

remove_node(Actor, Id, State) ->
    #state{nodes=Nodes} = State,
    Matches = lists:filter(
		fun(#{id := I}) ->
			   case I of
			       Id -> true;
			       _ -> false
			   end
		end, riak_dt_orswot:value(Nodes)),
    {ok, NewNodes} = riak_dt_orswot:update({remove_all, Matches}, Actor, Nodes),
    State#state{nodes=NewNodes}.
