-module(registrator_dns_test_stub).

%% Shared UDP stub responder for DNS test suites.
%%
%% Modes:
%%   {respond_with, Msg}                    -- decode inbound id, reply with Msg
%%   {respond_with_bytes, Bin}              -- send raw bytes back unchanged
%%   respond_with_id_mismatch               -- reply with id=0 (wrong id)
%%   drop                                   -- receive but never reply
%%   {respond_counted, Msg, CounterPid}     -- reply with Msg, send increment to CounterPid
%%   {capture_and_respond, CallerPid, Msg}  -- forward decoded query to CallerPid, reply with Msg

-include_lib("dns_erlang/include/dns.hrl").

-export([start/1, wait_for_port/0, stop/1]).

-define(PORT_WAIT_MS, 5000).

start(Mode) ->
    Self = self(),
    spawn(fun() -> stub_init(Self, Mode) end).

wait_for_port() ->
    receive
        {stub_ready, Port} -> {ok, Port}
    after ?PORT_WAIT_MS ->
        {error, stub_timeout}
    end.

stop(Pid) when is_pid(Pid) ->
    Pid ! stop;
stop(_) ->
    ok.

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------

stub_init(Parent, Mode) ->
    {ok, Sock} = gen_udp:open(0, [binary, {active, false}]),
    {ok, Port} = inet:port(Sock),
    Parent ! {stub_ready, Port},
    stub_loop(Sock, Mode).

stub_loop(Sock, drop) ->
    case gen_udp:recv(Sock, 0, infinity) of
        {ok, _} ->
            receive stop -> gen_udp:close(Sock) end;
        _ ->
            gen_udp:close(Sock)
    end;
stub_loop(Sock, Mode) ->
    receive
        stop ->
            gen_udp:close(Sock)
    after 0 ->
        case gen_udp:recv(Sock, 0, 100) of
            {ok, {Ip, Port, Bin}} ->
                handle_recv(Sock, Ip, Port, Bin, Mode),
                stub_loop(Sock, Mode);
            {error, timeout} ->
                stub_loop(Sock, Mode);
            _ ->
                gen_udp:close(Sock)
        end
    end.

handle_recv(Sock, Ip, Port, Bin, {respond_with, RespMsg}) ->
    InId = decode_query_id(Bin),
    Reply = RespMsg#dns_message{id = InId},
    gen_udp:send(Sock, Ip, Port, dns:encode_message(Reply));
handle_recv(Sock, Ip, Port, Bin, {respond_counted, RespMsg, CounterPid}) ->
    CounterPid ! increment,
    InId = decode_query_id(Bin),
    Reply = RespMsg#dns_message{id = InId},
    gen_udp:send(Sock, Ip, Port, dns:encode_message(Reply));
handle_recv(Sock, Ip, Port, _Bin, {respond_with_bytes, Bytes}) ->
    gen_udp:send(Sock, Ip, Port, Bytes);
handle_recv(Sock, Ip, Port, _Bin, respond_with_id_mismatch) ->
    WrongId = 0,
    Reply = #dns_message{id = WrongId, qr = true, rc = ?DNS_RCODE_NOERROR},
    gen_udp:send(Sock, Ip, Port, dns:encode_message(Reply));
handle_recv(Sock, Ip, Port, Bin, {capture_and_respond, CallerPid, RespMsg}) ->
    case dns:decode_message(Bin) of
        M when is_record(M, dns_message) ->
            CallerPid ! {captured_query, M},
            Reply = RespMsg#dns_message{id = M#dns_message.id},
            gen_udp:send(Sock, Ip, Port, dns:encode_message(Reply));
        {trailing_garbage, M, _} ->
            CallerPid ! {captured_query, M},
            Reply = RespMsg#dns_message{id = M#dns_message.id},
            gen_udp:send(Sock, Ip, Port, dns:encode_message(Reply))
    end;
handle_recv(_Sock, _Ip, _Port, _Bin, drop) ->
    ok.

decode_query_id(Bin) ->
    case dns:decode_message(Bin) of
        M when is_record(M, dns_message) -> M#dns_message.id;
        {trailing_garbage, M, _} -> M#dns_message.id
    end.
