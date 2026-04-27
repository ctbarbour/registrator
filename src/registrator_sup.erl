-module(registrator_sup).

-behavior(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    DNSServer = {registrator_dns_server,
		 {registrator_dns_server, start_link, []},
		 permanent, 5000, worker, [registrator_dns_server]},
    Nodes = {registrator_nodes,
	     {registrator_nodes, start_link, [groups()]},
	     permanent, 5000, worker, [registrator_nodes]},
    StateSync = {registrator_state_sync_server,
		 {registrator_state_sync_server, start_link,
		  [{0,0,0,0}, state_sync_port()]},
		 permanent, 5000, worker, [registrator_state_sync_server]},
    Opts = registrator_docker_opts(),
    DockerOpts = docker_opts(),
    Docker = {registrator_docker,
	      {registrator_docker, start_link, [Opts, DockerOpts]},
	      permanent, 5000, worker, [registrator_docker]},
    {ok, {{one_for_one, 5, 10}, [DNSServer, Nodes, StateSync, Docker]}}.

groups() ->
    application:get_env(registrator, groups, []).

%% Single listener on a single port. Picks the first group's state_sync_port,
%% or falls back to a default. Multi-group setups would need one listener per
%% group; out of scope for now.
state_sync_port() ->
    case groups() of
	[{_Name, #{state_sync_port := Port}} | _] -> Port;
	_ -> 6000
    end.

registrator_docker_opts() ->
    get_opts([advertise, refresh_ttl]).

docker_opts() ->
    case os:getenv("REGISTRATOR_DOCKER_SOCKET") of
	false ->
	    case application:get_env(registrator, docker_socket) of
		undefined  -> #{};
		{ok, Path} -> #{socket => Path}
	    end;
	Path ->
	    #{socket => Path}
    end.

get_opts(Keys) ->
    get_opts(Keys, []).

get_opts([], Acc) ->
    Acc;
get_opts([Opt | Rest], Acc) ->
    case application:get_env(registrator, Opt) of
	undefined ->
	    get_opts(Rest, Acc);
	{ok, Value} ->
	    get_opts(Rest, [{Opt, Value} | Acc])
    end.
