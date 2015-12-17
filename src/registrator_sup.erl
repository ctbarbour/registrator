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
	     {registrator_nodes, start_link, []},
	     permanent, 5000, worker, [registrator_nodes]},

    DockerOpts = case application:get_env(registrator, docker) of
		    undefined ->
			#{};
		    {ok, Value} ->
			Value
		end,
    Opts = registrator_docker_opts(),
    Docker = {registrator_docker,
	      {registrator_docker, start_link, [Opts, DockerOpts]},
	      permanent, 5000, worker, [registrator_docker]},
    {ok, {{one_for_one, 5, 10}, [DNSServer, Nodes, Docker]}}.

registrator_docker_opts() ->
    registrator_docker_opts([advertise, refresh_ttl], []).

registrator_docker_opts([], Acc) ->
    Acc;
registrator_docker_opts([Opt | Rest], Acc) ->
    case application:get_env(registrator, Opt) of
	undefined ->
	    registrator_docker_opts(Rest, Acc);
	{ok, Value} ->
	    [{Opt, Value} | Acc]
    end.
