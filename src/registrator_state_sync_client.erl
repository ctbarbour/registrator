-module(registrator_state_sync_client).

%% One-shot TCP client that pulls anti-entropy state from a peer.
%%
%% Wire protocol matches registrator_state_sync_server.

-export([pull/3]).

-define(CONNECT_TIMEOUT, 5000).

-spec pull(inet:ip_address(), inet:port_number(), pos_integer()) ->
	  {ok, registrator_replica:sync_payload()} | {error, term()}.
pull(Ip, Port, Timeout) ->
    Opts = [binary, {packet, 4}, {active, false}, {nodelay, true}],
    case gen_tcp:connect(Ip, Port, Opts, ?CONNECT_TIMEOUT) of
	{ok, Sock} ->
	    try
		Req = term_to_binary({sync_request_v1, my_actor()}),
		case gen_tcp:send(Sock, Req) of
		    ok ->
			case gen_tcp:recv(Sock, 0, Timeout) of
			    {ok, Frame} ->
				case safe_term(Frame) of
				    {sync_response_v1, Payload} ->
					{ok, Payload};
				    _ ->
					{error, bad_response}
				end;
			    {error, Reason} ->
				{error, {recv, Reason}}
			end;
		    {error, Reason} ->
			{error, {send, Reason}}
		end
	    after
		gen_tcp:close(Sock)
	    end;
	{error, Reason} ->
	    {error, {connect, Reason}}
    end.

%% Placeholder: the protocol carries the requesting actor for future use
%% (e.g. a server can short-circuit if it knows it has nothing new for the
%% caller). For now the server doesn't inspect it, so an empty actor is fine.
my_actor() ->
    <<>>.

safe_term(Bin) ->
    try binary_to_term(Bin, [safe])
    catch error:badarg -> {error, bad_term}
    end.
