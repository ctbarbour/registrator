-module(discovery).

-include_lib("kernel/src/inet_dns.hrl").

-export([discover/1, discover/2]).

discover(Domain) ->
    discover(Domain, {{172,17,0,1}, 53}).

discover(Domain, {Ip, Port}) ->
    {ok, Socket} = gen_udp:open(1337, [binary, {reuseaddr, true}, {active, true}]),
    Query = #dns_rec{header = #dns_header{id = 0,qr = 0,opcode = 0,
					  aa = 0,tc = 0,rd = 0,ra = 0,pr = 0,
					  rcode = 0},
		     qdlist = [#dns_query{domain = Domain,
					  type = srv,class = in}],
		     anlist = [],nslist = [],arlist = []},
    ok = gen_udp:send(Socket, Ip, Port, inet_dns:encode(Query)),
    receive
	{udp, Socket, _Ip, _Port, Body} ->
	    {ok, Reply} = inet_dns:decode(Body),
	    Services = parse_reply(Reply),
	    ok = gen_udp:close(Socket),
	    {ok, Services}
    after
	1000 ->
	    ok = gen_udp:close(Socket),
	    {error, timeout}
    end.

parse_reply(#dns_rec{anlist=Answers}) ->
    lists:map(fun(#dns_rr{data=Data}) ->
		      {_W, _P, Port, Target} = Data,
		      {ok, Address} = inet_parse:address(Target),
		      {Address, Port}
	      end, Answers).
