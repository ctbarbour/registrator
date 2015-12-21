-module(registrator_docker).

-export([start_link/2]).
-export([init/3, state_from_opts/2, system_continue/3, system_terminate/4,
	system_code_change/4]).

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
	  parent               :: pid(),
	  docker               :: pid(),
	  docker_opts          :: maps:map(),
	  event_ref            :: reference(),
	  dead_containers = [] :: [dead_container()],
	  refresh_ref          :: reference(),
	  refresh_ttl = 30000  :: pos_integer(),
	  connection_retries = 3 :: pos_integer(),
	  advertise            :: inet:ip_address()
	 }).

-type state() :: #state{}.

start_link(Opts, DockerOpts) when is_list(DockerOpts) ->
    start_link(Opts, maps:from_list(DockerOpts));
start_link(Opts, DockerOpts) when is_map(DockerOpts) ->
    proc_lib:start_link(?MODULE, init,
			[self(), Opts, DockerOpts]).

state_from_opts([], State) ->
    State;
state_from_opts([{advertise, Value} | Opts], State) ->
    state_from_opts(Opts, State#state{advertise=Value});
state_from_opts([{refresh_ttl, Value} | Opts], State) ->
    state_from_opts(Opts, State#state{refresh_ttl=Value});
state_from_opts([{connection_retries, Value} | Opts], State) ->
    state_from_opts(Opts, State#state{connection_retries=Value}).

send_refresh(State) ->
    #state{refresh_ttl=Ttl} = State,
    erlang:send_after(Ttl, self(), refresh).

init(Parent, Opts, DockerOpts) ->
    ok = proc_lib:init_ack(Parent, {ok, self()}),
    ok = error_logger:info_msg("Parent: ~p", [Parent]),
    State = state_from_opts(Opts, #state{}),
    connect(State#state{parent=Parent, docker_opts=DockerOpts},
	    State#state.connection_retries).

connect(State, Retries) ->
    #state{docker_opts=DockerOpts} = State,
    case nkdocker:start_link(DockerOpts) of
	{ok, Docker} ->
	    Filter = #{filters => #{event => [start, stop, die, kill]}},
	    {async, EventRef} = nkdocker:events(Docker, Filter),
	    before_loop(State#state{docker=Docker, event_ref=EventRef});
	{error, Reason} ->
	    ok = error_logger:error_msg("Error connection: ~w~n", [Reason]),
	    retry(State, Retries)
    end.

before_loop(State) ->
    NewState = sync(State),
    RefreshRef = send_refresh(NewState),
    loop(State#state{refresh_ref=RefreshRef}).

retry(_State, 0) ->
    ok;
retry(#state{refresh_ref=RefreshRef} = State, Retries)
  when is_reference(RefreshRef) ->
    _ = erlang:cancel_timer(RefreshRef),
    receive
	refresh ->
	    ok
    after 0 ->
	    ok
    end,
    retry_loop(State#state{refresh_ref=undefined}, Retries);
retry(State, Retries) ->
    retry_loop(State, Retries).

retry_loop(_State, 0) ->
    error(gone);
retry_loop(State, Retries) ->
    #state{parent=Parent} = State,
    _ = erlang:send_after(5000, self(), retry),
    receive
	retry ->
	    connect(State, Retries - 1);
	{system, From, Request} ->
	    sys:handle_system_msg(Request, From, Parent, ?MODULE, [],
				  {retry_loop, State, Retries})
    end.

loop(State) ->
    #state{docker=Docker, event_ref=EventRef} = State,
    receive
	refresh ->
	    #state{dead_containers = DeadContainers, refresh_ttl=Ttl} = State,
	    NewDeadContainers = filter_dead_containers(DeadContainers, Ttl),
	    _ = sync(State),
	    RefreshRef = send_refresh(State),
	    loop(State#state{refresh_ref=RefreshRef,
			     dead_containers=NewDeadContainers});
	{nkdocker, EventRef, {data, Event}} ->
	    loop(handle_event(Event, State));
	{'EXIT', Docker, Reason} ->
	    down(State, Reason);
	{system, From, Request} ->
	    #state{parent=Parent} = State,
	    sys:handle_system_msg(Request, From, Parent, ?MODULE, [],
				  {loop, State});
	Any ->
	    error_logger:error_msg("Unexpected message: ~w~n", [Any]),
	    loop(State)
    end.

down(State, shutdown) ->
    loop(State#state{docker=undefined});
down(State, normal) ->
    loop(State#state{docker=undefined});
down(State, _Reason) ->
    #state{connection_retries=Retries} = State,
    retry(State#state{docker=undefined}, Retries).

sync(State) ->
    #state{docker=Docker} = State,
    {ok, Containers} = nkdocker:ps(Docker),
    RegisteredNodes = lists:foldl(fun(#{id := Id}, Acc) ->
					  sets:add_element(Id, Acc)
				  end, sets:new(), registrator_nodes:lookup()),
    RunningContainers = lists:foldl(fun(#{<<"Id">> := Id}, Acc) ->
					    sets:add_element(Id, Acc)
				    end, sets:new(), Containers),
    Deregister = sets:subtract(RegisteredNodes, RunningContainers),
    State1 = lists:foldl(
	       fun(Id, S) ->
		       remove(Id, S)
	       end, State, sets:to_list(Deregister)),
    Register = sets:subtract(RunningContainers, RegisteredNodes),
    lists:foldl(
      fun(Id, S) ->
	      add(Id, S)
      end, State1, sets:to_list(Register)).

filter_dead_containers(DeadContainers, Ttl) ->
    lists:filtermap(fun(DeadContainer) ->
			    filter_dead_container(DeadContainer, Ttl)
		    end, DeadContainers).

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
    #state{docker=Docker, dead_containers=DeadContainers, refresh_ttl=Ttl} = State,
    {ok, Container} = nkdocker:inspect(Docker, ContainerId),
    NewDeadContainers = filter_dead_containers(DeadContainers, Ttl),
    ServicePorts = service_ports(Container, State),
    Services = lists:map(
		 fun(ServicePort) ->
			 new_service(ServicePort, Container)
		 end, ServicePorts),
    _ = lists:map(
	  fun(Service) ->
		  registrator_nodes:register(Service)
	  end, Services),
    State#state{dead_containers = NewDeadContainers}.

-spec service_ports(maps:map(), state()) -> [service_port()].
service_ports(Container, State) ->
    #{<<"HostConfig">> := #{<<"PortBindings">> := PortBindings}} = Container,
    #{<<"NetworkSettings">> := #{<<"Ports">> := Ports}} = Container,
    maps:fold(
      fun(Port, Published, Acc) ->
	      service_port(Port, Published, State, Acc)
      end, [], maps:merge(PortBindings, Ports)).

-spec service_port(binary(), [maps:map()], state(), [service_port()])
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
    parse_ip(Advertised).

exposed_port([ExposedPort]) ->
    [ExposedPort, <<"tcp">>];
exposed_port([ExposedPort, PortType]) ->
    [ExposedPort, PortType].

parse_ip(MaybeIp) when is_binary(MaybeIp) ->
    parse_ip(binary_to_list(MaybeIp));
parse_ip(MaybeIp) when is_list(MaybeIp) ->
    {ok, Ip} = inet_parse:address(MaybeIp),
    Ip.

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
      end, #{}, Env).

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
    #state{docker = Docker, dead_containers = DeadContainers,
	   refresh_ttl=Ttl} = State,
    MaybeContainer = nkdocker:inspect(Docker, Id),
    case {did_exit_cleanly(MaybeContainer), registrator_nodes:lookup(Id)} of
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
	#{<<"Running">> := false, <<"ExitCode">> := 0} ->
	    true;
	_ ->
	    false
    end;
did_exit_cleanly({error, {not_found, _Reason}}) ->
    false.

system_continue(_, _, {retry_loop, State, Retry}) ->
    retry_loop(State, Retry);
system_continue(_, _, {loop, State}) ->
    loop(State).

system_terminate(Reason, _, _, _) ->
    exit(Reason).

system_code_change(Misc, _, _, _) ->
    {ok, Misc}.
