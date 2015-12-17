-module(registrator_dns_server).

-include_lib("dns_erlang/include/dns.hrl").

-behavior(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {socket}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, Socket} = gen_udp:open(5555, [binary, {active, once},
				       {read_packets, 1000},
				       {reuseaddr, true}]),
    {ok, #state{socket=Socket}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({udp, Socket, Host, Port, Packet}, State) ->
    _ = handle_udp_dns_query(Socket, Host, Port, Packet),
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_udp_dns_query(Socket, Host, Port, Packet) ->
    case dns:decode_message(Packet) of
	{trailing_garbage, DecodedMessage, _} ->
	    handle_decoded_message(DecodedMessage, Socket, Host, Port);
	{_Error, _, _} ->
	    ok;
	DecodedMessage ->
	    handle_decoded_message(DecodedMessage, Socket, Host, Port)
    end.

handle_decoded_message(DecodedMessage, Socket, Host, Port) ->
    Response = registrator_dns_resolver:resolve(DecodedMessage),
    case dns:encode_message(Response, [{max_size, 512}]) of
	{false, EncodedMessage} ->
	    gen_udp:send(Socket, Host, Port, EncodedMessage);
	{true, EncodedMessage, Message} when is_record(Message, dns_message) ->
	    gen_udp:send(Socket, Host, Port, EncodedMessage);
	{false, EncodedMessage, _TsigMac} ->
	    gen_udp:send(Socket, Host, Port, EncodedMessage);
	{true, EncodedMessage, _TsigMac, _Message} ->
	    gen_udp:send(Socket, Host, Port, EncodedMessage)
    end.
