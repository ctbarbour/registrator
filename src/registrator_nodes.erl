-module(registrator_nodes).

-behavior(gen_server).

-export([start_link/0, register/1, lookup/0, lookup/1, lookup/2,
	 unregister/1]).
-export([init/1, handle_call/3, handle_info/2, handle_cast/2, code_change/3,
	 terminate/2]).

-record(state, {nodes}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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

init([]) ->
    {ok, #state{nodes = []}}.

handle_call(lookup, _From, State) ->
    #state{nodes=Nodes} = State,
    {reply, Nodes, State};
handle_call({lookup, Id}, _From, State) ->
    #state{nodes=Nodes} = State,
    Matches = lists:filter(
		fun(#{id := I}) ->
			   case I of
			       Id -> true;
			       _ -> false
			   end
		end, Nodes),
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
		end, Nodes),
    {reply, Matches, State};
handle_call({register, Node}, _From, State) ->
    #state{nodes=Nodes} = State,
    {reply, ok, State#state{nodes=[Node | Nodes]}};
handle_call({unregister, Id}, _From, State) ->
    #state{nodes=Nodes} = State,
    Keep = lists:filter(fun(E) ->
				#{id := I} = E,
				case I of
				    Id -> false;
				    _ -> true
				end
			end, Nodes),
    {reply, ok, State#state{nodes=Keep}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
