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
	     {registrator_nodes, start_link, swim_opts()},
	     permanent, 5000, worker, [registrator_nodes]},

    Opts = registrator_docker_opts(),
    DockerOpts = application:get_all_env(nkdocker),
    Docker = {registrator_docker,
	      {registrator_docker, start_link, [Opts, DockerOpts]},
	      permanent, 5000, worker, [registrator_docker]},
    {ok, {{one_for_one, 5, 10}, [DNSServer, Nodes, Docker]}}.

swim_opts() ->
    {ok, Actor} = application:get_env(registrator, actor),
    [Actor, application:get_env(registrator, groups, [])].

registrator_docker_opts() ->
    get_opts([advertise, refresh_ttl]).

get_opts(Keys) ->
    get_opts(Keys, []).

get_opts([], Acc) ->
    Acc;
get_opts([Opt | Rest], Acc) ->
    case application:get_env(registrator, Opt) of
	undefined ->
	    get_opts(Rest, Acc);
	{ok, Value} ->
	    [{Opt, Value} | Acc]
    end.
