-module(registrator_docker).

-behavior(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
	 terminate/2]).

-record(dead_container, {
	  id  :: binary(),
	  ttl :: pos_integer()
	 }).

-type dead_container() :: #dead_container{}.

-record(service_port, {
	  exposed_port :: inet:port_number(),
	  host_port    :: inet:port_number(),
	  host_ip      :: inet:ip_address(),
	  port_type    :: binary()
	 }).

-type service_port() :: #service_port{}.

-record(state, {
	  events_conn           :: pid() | undefined,
	  events_mref           :: reference() | undefined,
	  events_stream         :: gun:stream_ref() | undefined,
	  events_buffer = <<>>  :: binary(),
	  socket_path           :: string(),
	  dead_containers = []  :: [dead_container()],
	  refresh_ref           :: reference() | undefined,
	  refresh_ttl = 30000   :: pos_integer(),
	  advertise             :: inet:ip_address() | undefined
	 }).

-type state() :: #state{}.

-define(DEFAULT_SOCKET, "/var/run/docker.sock").
-define(EVENT_FILTER_QUERY,
        "/events?filters="
        "%7B%22event%22%3A%5B%22start%22%2C%22stop%22%2C%22die%22%2C%22kill%22%5D%7D").
-define(REQUEST_TIMEOUT, 10000).

start_link(Opts, DockerOpts) when is_list(DockerOpts) ->
    start_link(Opts, maps:from_list(DockerOpts));
start_link(Opts, DockerOpts) when is_map(DockerOpts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Opts, DockerOpts], []).

state_from_opts([], State) ->
    State;
state_from_opts([{advertise, Value} | Opts], State) ->
    state_from_opts(Opts, State#state{advertise=parse_ip(Value)});
state_from_opts([{refresh_ttl, Value} | Opts], State) ->
    state_from_opts(Opts, State#state{refresh_ttl=Value});
state_from_opts([{_K, _V} | Opts], State) ->
    state_from_opts(Opts, State).

send_refresh(#state{refresh_ttl=Ttl}) ->
    erlang:send_after(Ttl, self(), refresh).

init([Opts, DockerOpts]) ->
    process_flag(trap_exit, true),
    SocketPath = maps:get(socket, DockerOpts, ?DEFAULT_SOCKET),
    State0 = state_from_opts(Opts, #state{socket_path=SocketPath}),
    State1 = open_docker(State0),
    State2 = sync(State1),
    RefreshRef = send_refresh(State2),
    {ok, State2#state{refresh_ref=RefreshRef}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(refresh, State) ->
    #state{dead_containers = DeadContainers, refresh_ttl=Ttl} = State,
    NewDeadContainers = filter_dead_containers(DeadContainers, Ttl),
    State1 = sync(State),
    RefreshRef = send_refresh(State1),
    {noreply, State1#state{refresh_ref=RefreshRef,
			   dead_containers=NewDeadContainers}};
handle_info({gun_up, _Conn, _Protocol}, State) ->
    {noreply, State};
handle_info({gun_response, Conn, StreamRef, _IsFin, Status, _Headers},
	    #state{events_conn=Conn, events_stream=StreamRef} = State) ->
    case Status of
	200 ->
	    {noreply, State};
	_ ->
	    error_logger:warning_msg("Docker /events returned status ~p", [Status]),
	    {noreply, State}
    end;
handle_info({gun_data, Conn, StreamRef, IsFin, Data},
	    #state{events_conn=Conn, events_stream=StreamRef} = State) ->
    State1 = consume_event_data(Data, State),
    case IsFin of
	fin ->
	    error_logger:warning_msg("Docker /events stream ended, reconnecting"),
	    {noreply, reopen_event_stream(State1)};
	nofin ->
	    {noreply, State1}
    end;
handle_info({gun_error, _Conn, _StreamRef, Reason}, State) ->
    error_logger:warning_msg("gun stream error: ~p", [Reason]),
    {noreply, State};
handle_info({gun_down, Conn, _Protocol, _Reason, _KilledStreams},
	    #state{events_conn=Conn} = State) ->
    {noreply, reopen_events(State)};
handle_info({gun_down, _Conn, _Protocol, _Reason, _KilledStreams}, State) ->
    {noreply, State};
handle_info({'DOWN', MRef, process, _Pid, Reason},
	    #state{events_mref=MRef} = State) ->
    error_logger:warning_msg("Docker events connection down: ~p", [Reason]),
    {noreply, reopen_events(State)};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    catch gun:close(State#state.events_conn),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ===================================================================
%%% Docker connection / event stream
%%% ===================================================================

open_docker(State) ->
    open_events(State).

open_events(#state{socket_path=SocketPath} = State) ->
    {ok, Conn} = gun:open_unix(SocketPath, #{transport => tcp,
					     protocols => [http]}),
    {ok, _Protocol} = gun:await_up(Conn, ?REQUEST_TIMEOUT),
    MRef = erlang:monitor(process, Conn),
    StreamRef = gun:get(Conn, ?EVENT_FILTER_QUERY,
			[{<<"host">>, <<"docker">>},
			 {<<"accept">>, <<"application/json">>}]),
    State#state{events_conn=Conn, events_mref=MRef,
		events_stream=StreamRef, events_buffer = <<>>}.

reopen_events(State) ->
    open_events(State#state{events_conn=undefined, events_mref=undefined,
			    events_stream=undefined, events_buffer = <<>>}).

reopen_event_stream(#state{events_conn=Conn} = State) when is_pid(Conn) ->
    StreamRef = gun:get(Conn, ?EVENT_FILTER_QUERY,
			[{<<"host">>, <<"docker">>},
			 {<<"accept">>, <<"application/json">>}]),
    State#state{events_stream=StreamRef, events_buffer = <<>>};
reopen_event_stream(State) ->
    reopen_events(State).

consume_event_data(Data, #state{events_buffer=Buf} = State) ->
    {Events, Rest} = split_lines(<<Buf/binary, Data/binary>>, []),
    State1 = lists:foldl(fun decode_and_handle/2, State, Events),
    State1#state{events_buffer=Rest}.

split_lines(Bin, Acc) ->
    case binary:split(Bin, <<"\n">>) of
	[Complete, Tail] ->
	    split_lines(Tail, [Complete | Acc]);
	[Partial] ->
	    {lists:reverse(Acc), Partial}
    end.

decode_and_handle(<<>>, State) ->
    State;
decode_and_handle(Line, State) ->
    try json:decode(Line) of
	Event when is_map(Event) ->
	    handle_event(Event, State);
	_ ->
	    State
    catch
	error:_ ->
	    error_logger:warning_msg("Could not decode docker event: ~p", [Line]),
	    State
    end.

%%% ===================================================================
%%% Docker REST calls (one-shot)
%%% ===================================================================

list_containers(#state{socket_path=SocketPath}) ->
    request_json(SocketPath, <<"/containers/json">>).

inspect_container(#state{socket_path=SocketPath}, Id) when is_binary(Id) ->
    request_json(SocketPath, <<"/containers/", Id/binary, "/json">>).

%% Each REST call uses a fresh connection. Podman's compat API closes
%% connections aggressively, and the cost of a UDS connect is negligible.
request_json(SocketPath, Path) ->
    case gun:open_unix(SocketPath, #{transport => tcp, protocols => [http]}) of
	{ok, Conn} ->
	    try
		{ok, _} = gun:await_up(Conn, ?REQUEST_TIMEOUT),
		do_request(Conn, Path)
	    after
		gun:close(Conn)
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

do_request(Conn, Path) ->
    StreamRef = gun:get(Conn, Path,
			[{<<"host">>, <<"docker">>},
			 {<<"accept">>, <<"application/json">>}]),
    case gun:await(Conn, StreamRef, ?REQUEST_TIMEOUT) of
	{response, fin, Status, _Headers} ->
	    {error, {http_status, Status, <<>>}};
	{response, nofin, Status, _Headers} when Status >= 200, Status < 300 ->
	    case gun:await_body(Conn, StreamRef, ?REQUEST_TIMEOUT) of
		{ok, Body} ->
		    {ok, json:decode(Body)};
		{error, Reason} ->
		    {error, Reason}
	    end;
	{response, nofin, Status, _Headers} ->
	    case gun:await_body(Conn, StreamRef, ?REQUEST_TIMEOUT) of
		{ok, Body} -> {error, {http_status, Status, Body}};
		{error, Reason} -> {error, {http_status, Status, Reason}}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

%%% ===================================================================
%%% Sync / event handling
%%% ===================================================================

sync(State) ->
    #state{advertise=Advertise} = State,
    case list_containers(State) of
	{ok, Containers} when is_list(Containers) ->
	    Services = lists:filter(fun(#{address := Address}) ->
					    Address =:= Advertise
				    end, registrator_nodes:lookup()),
	    RegisteredNodes = lists:foldl(fun(#{id := Id}, Acc) ->
						  sets:add_element(Id, Acc)
					  end, sets:new(), Services),
	    RunningContainers = lists:foldl(fun(#{<<"Id">> := Id}, Acc) ->
						    sets:add_element(Id, Acc)
					    end, sets:new(), Containers),
	    Deregister = sets:subtract(RegisteredNodes, RunningContainers),
	    State1 = lists:foldl(
		       fun(Id, S) -> remove(Id, S) end,
		       State, sets:to_list(Deregister)),
	    Register = sets:subtract(RunningContainers, RegisteredNodes),
	    lists:foldl(
	      fun(Id, S) -> add(Id, S) end,
	      State1, sets:to_list(Register));
	{error, Reason} ->
	    error_logger:warning_msg("Docker list_containers failed: ~p", [Reason]),
	    State
    end.

filter_dead_containers(DeadContainers, Ttl) ->
    lists:filtermap(fun(DC) -> filter_dead_container(DC, Ttl) end,
		    DeadContainers).

filter_dead_container(#dead_container{ttl=ContainerTtl}, Ttl)
  when Ttl - ContainerTtl =< 0 ->
    false;
filter_dead_container(DeadContainer, Ttl) ->
    #dead_container{ttl=ContainerTtl} = DeadContainer,
    {true, DeadContainer#dead_container{ttl=Ttl - ContainerTtl}}.

handle_event(#{<<"status">> := <<"start">>} = Event, State) ->
    #{<<"id">> := Id} = Event,
    ok = error_logger:info_msg("Started container ~p", [Id]),
    add(Id, State);
handle_event(#{<<"status">> := <<"stop">>} = Event, State) ->
    #{<<"id">> := ContainerId} = Event,
    ok = error_logger:info_msg("Stopped container ~p", [ContainerId]),
    remove(ContainerId, State);
handle_event(#{<<"status">> := Status} = Event, State)
  when Status =:= <<"die">>; Status =:= <<"kill">> ->
    #{<<"id">> := ContainerId} = Event,
    ok = error_logger:info_msg("~p container ~p", [Status, ContainerId]),
    remove_on_exit(ContainerId, State);
handle_event(_Event, State) ->
    State.

add(ContainerId, State) ->
    #state{dead_containers=DeadContainers, refresh_ttl=Ttl} = State,
    case inspect_container(State, ContainerId) of
	{ok, Container} ->
	    NewDeadContainers = filter_dead_containers(DeadContainers, Ttl),
	    ServicePorts = service_ports(Container, State),
	    Services = [new_service(SP, Container) || SP <- ServicePorts],
	    _ = [registrator_nodes:register(S) || S <- Services],
	    State#state{dead_containers = NewDeadContainers};
	{error, Reason} ->
	    error_logger:warning_msg("Inspect ~p failed: ~p",
				     [ContainerId, Reason]),
	    State
    end.

fetch_port_bindings(#{<<"HostConfig">> := #{<<"PortBindings">> := null}}) ->
    #{};
fetch_port_bindings(#{<<"HostConfig">> := #{<<"PortBindings">> := PortBindings}})
  when is_map(PortBindings) ->
    PortBindings;
fetch_port_bindings(_) ->
    #{}.

fetch_ports(#{<<"NetworkSettings">> := #{<<"Ports">> := null}}) ->
    #{};
fetch_ports(#{<<"NetworkSettings">> := #{<<"Ports">> := Ports}})
  when is_map(Ports) ->
    Ports;
fetch_ports(_) ->
    #{}.

-spec service_ports(map(), state()) -> [service_port()].
service_ports(Container, State) ->
    PortBindings = fetch_port_bindings(Container),
    Ports = fetch_ports(Container),
    maps:fold(
      fun(Port, Published, Acc) ->
	      service_port(Port, Published, State, Acc)
      end, [], maps:merge(PortBindings, Ports)).

-spec service_port(binary(), [map()] | null, state(), [service_port()])
		  -> [service_port()].
service_port(_Port, null, _State, Acc) ->
    Acc;
service_port(_Port, [], _State, Acc) ->
    Acc;
service_port(Port, [Published|_], State, Acc) ->
    #{<<"HostPort">> := HostPort, <<"HostIp">> := MaybeHostIp} = Published,
    [ExposedPort, PortType] = exposed_port(binary:split(Port, <<"/">>)),
    HostIp = host_ip(MaybeHostIp, State),
    ServicePort = #service_port{exposed_port=parse_port(ExposedPort),
				host_port=parse_port(HostPort),
				host_ip=HostIp,
				port_type=PortType},
    [ServicePort | Acc].

host_ip(MaybeHostIp, #state{advertise=undefined}) ->
    parse_ip(MaybeHostIp);
host_ip(_MaybeHostIp, #state{advertise=Advertised}) ->
    Advertised.

exposed_port([ExposedPort]) ->
    [ExposedPort, <<"tcp">>];
exposed_port([ExposedPort, PortType]) ->
    [ExposedPort, PortType].

parse_ip(MaybeIp) when is_binary(MaybeIp) ->
    parse_ip(binary_to_list(MaybeIp));
parse_ip(MaybeIp) when is_list(MaybeIp) ->
    {ok, Ip} = inet_parse:address(MaybeIp),
    Ip;
parse_ip(MaybeIp) when is_tuple(MaybeIp) ->
    MaybeIp.

parse_port(Port) when is_binary(Port) ->
    binary_to_integer(Port);
parse_port(Port) when is_list(Port) ->
    list_to_integer(Port);
parse_port(Port) when is_integer(Port) ->
    Port.

default_name(#{<<"Config">> := #{<<"Image">> := Image}}) ->
    Base = hd(lists:reverse(binary:split(Image, <<"/">>, [global]))),
    hd(binary:split(Base, <<":">>, [global])).

new_service(ServicePort, Container) ->
    #service_port{host_ip=HostIp, host_port=HostPort,
		  port_type=DefaultPortType} = ServicePort,
    #{<<"Id">> := ContainerId} = Container,
    DefaultName = default_name(Container),
    ServiceMetadata = service_metadata(ServicePort, Container),
    ServiceName = maps:get("name", ServiceMetadata, DefaultName),
    Protocol = maps:get("proto", ServiceMetadata, DefaultPortType),
    #{id => ContainerId, address => HostIp,
      port => HostPort, service => ServiceName, protocol => Protocol}.

service_metadata(_ServicePort, #{<<"Config">> := #{<<"Env">> := null}}) ->
    #{};
service_metadata(ServicePort, #{<<"Config">> := #{<<"Env">> := Env}}) ->
    #service_port{exposed_port=ExposedPort} = ServicePort,
    Regexp = ["^SERVICE_", integer_to_list(ExposedPort),
	      "_(?<ATTR>[a-zA-Z_]+)=(?<VALUE>.*)$"],
    lists:foldl(
      fun(E, Acc) ->
	      MaybeMatch = re:run(E, Regexp,
				  [anchored, {capture, all_names, binary}]),
	      append_service_metadata(MaybeMatch, Acc)
      end, #{}, Env);
service_metadata(_ServicePort, _Container) ->
    #{}.

append_service_metadata({match, [Attr, Value]}, Metadata) ->
    maps:put(string:to_lower(binary_to_list(Attr)), Value, Metadata);
append_service_metadata(nomatch, Metadata) ->
    Metadata.

remove(Id, State) ->
    #state{dead_containers=DeadContainers, refresh_ttl=Ttl} = State,
    NewDeadContainers = filter_dead_containers(DeadContainers, Ttl),
    ok = registrator_nodes:unregister(Id),
    State#state{dead_containers = NewDeadContainers}.

remove_on_exit(Id, State) ->
    #state{dead_containers = DeadContainers, refresh_ttl=Ttl} = State,
    case {did_exit_cleanly(inspect_container(State, Id)),
	  registrator_nodes:lookup(Id)} of
	{true, _Services} ->
	    remove(Id, State);
	{false, []} ->
	    State;
	{false, _Services} ->
	    DeadContainer = #dead_container{id=Id, ttl=Ttl},
	    State#state{dead_containers=[DeadContainer | DeadContainers]}
    end.

did_exit_cleanly({ok, #{<<"State">> := ContainerState}}) ->
    case ContainerState of
	#{<<"Running">> := false, <<"ExitCode">> := 0} -> true;
	_ -> false
    end;
did_exit_cleanly({error, _Reason}) ->
    false.
