-module(registrator_dns_recursor_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("dns_erlang/include/dns.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    empty_list_returns_no_recursors/1,
    success_returns_upstream_message_with_ra_true_aa_false_and_orig_id/1,
    success_preserves_upstream_servfail_rcode_passthrough/1,
    success_preserves_upstream_nxdomain_passthrough/1,
    success_preserves_upstream_ttls_verbatim/1,
    timeout_falls_through_to_next_recursor/1,
    all_recursors_fail_returns_all_failed/1,
    id_mismatch_treated_as_failure/1,
    qr_false_treated_as_failure/1,
    socket_closed_on_success_and_failure/1
]).

%%--------------------------------------------------------------------
%% Suite setup/teardown
%%--------------------------------------------------------------------

all() ->
    [
        empty_list_returns_no_recursors,
        success_returns_upstream_message_with_ra_true_aa_false_and_orig_id,
        success_preserves_upstream_servfail_rcode_passthrough,
        success_preserves_upstream_nxdomain_passthrough,
        success_preserves_upstream_ttls_verbatim,
        timeout_falls_through_to_next_recursor,
        all_recursors_fail_returns_all_failed,
        id_mismatch_treated_as_failure,
        qr_false_treated_as_failure,
        socket_closed_on_success_and_failure
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(timeout_falls_through_to_next_recursor, Config) ->
    %% Two stubs: first drops (timeout), second answers normally
    Drop = start_stub(drop),
    {ok, DropPort} = wait_for_stub_port(),
    Answer = start_stub({respond_with, make_noerror_response()}),
    {ok, AnswerPort} = wait_for_stub_port(),
    [{stub1_pid, Drop}, {stub1_port, DropPort},
     {stub2_pid, Answer}, {stub2_port, AnswerPort} | Config];
init_per_testcase(id_mismatch_treated_as_failure, Config) ->
    %% Two sub-cases; start two stubs
    Mismatch = start_stub(respond_with_id_mismatch),
    {ok, MismatchPort} = wait_for_stub_port(),
    Valid = start_stub({respond_with, make_noerror_response()}),
    {ok, ValidPort} = wait_for_stub_port(),
    [{stub1_pid, Mismatch}, {stub1_port, MismatchPort},
     {stub2_pid, Valid}, {stub2_port, ValidPort} | Config];
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(timeout_falls_through_to_next_recursor, Config) ->
    stop_stub(proplists:get_value(stub1_pid, Config)),
    stop_stub(proplists:get_value(stub2_pid, Config)),
    Config;
end_per_testcase(id_mismatch_treated_as_failure, Config) ->
    stop_stub(proplists:get_value(stub1_pid, Config)),
    stop_stub(proplists:get_value(stub2_pid, Config)),
    Config;
end_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Stub UDP responder helpers
%%--------------------------------------------------------------------

%% Modes:
%%   {respond_with, Msg}       -- answer with Msg (copy id from inbound)
%%   {respond_with_bytes, Bin} -- send raw bytes back
%%   respond_with_id_mismatch  -- answer with wrong id
%%   drop                      -- read but never reply

start_stub(Mode) ->
    Self = self(),
    Pid = spawn(fun() -> stub_init(Self, Mode) end),
    Pid.

wait_for_stub_port() ->
    receive
        {stub_ready, Port} -> {ok, Port}
    after 5000 ->
        {error, stub_timeout}
    end.

stop_stub(Pid) when is_pid(Pid) ->
    Pid ! stop;
stop_stub(_) ->
    ok.

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
                handle_stub_recv(Sock, Ip, Port, Bin, Mode),
                stub_loop(Sock, Mode);
            {error, timeout} ->
                stub_loop(Sock, Mode);
            _ ->
                gen_udp:close(Sock)
        end
    end.

handle_stub_recv(Sock, Ip, Port, Bin, {respond_with, RespMsg}) ->
    InId = case dns:decode_message(Bin) of
        M when is_record(M, dns_message) -> M#dns_message.id;
        {trailing_garbage, M, _} -> M#dns_message.id
    end,
    %% Copy qr from RespMsg so tests can control it; copy the inbound id
    Reply = RespMsg#dns_message{id = InId},
    RespBin = dns:encode_message(Reply),
    gen_udp:send(Sock, Ip, Port, RespBin);
handle_stub_recv(Sock, Ip, Port, _Bin, {respond_with_bytes, Bytes}) ->
    gen_udp:send(Sock, Ip, Port, Bytes);
handle_stub_recv(Sock, Ip, Port, _Bin, respond_with_id_mismatch) ->
    %% Send a response with a deliberately wrong id (0 is very unlikely to match random_id())
    WrongId = 0,
    Reply = (make_noerror_response())#dns_message{id = WrongId, qr = true},
    gen_udp:send(Sock, Ip, Port, dns:encode_message(Reply));
handle_stub_recv(_Sock, _Ip, _Port, _Bin, drop) ->
    ok.

%%--------------------------------------------------------------------
%% Message helpers
%%--------------------------------------------------------------------

make_query() ->
    Question = #dns_query{name = <<"upstream.example.">>, type = ?DNS_TYPE_A,
                          class = ?DNS_CLASS_IN},
    #dns_message{id = 1234, questions = [Question]}.

make_noerror_response() ->
    Answers = [#dns_rr{
        name = <<"upstream.example.">>,
        type = ?DNS_TYPE_A,
        ttl = 300,
        data = #dns_rrdata_a{ip = {1, 2, 3, 4}}
    }],
    #dns_message{
        qr = true,
        rc = ?DNS_RCODE_NOERROR,
        ra = false,
        aa = false,
        anc = length(Answers),
        answers = Answers
    }.

%%--------------------------------------------------------------------
%% Test cases
%%--------------------------------------------------------------------

empty_list_returns_no_recursors(_Config) ->
    Msg = make_query(),
    {error, no_recursors} = registrator_dns_recursor:forward(Msg, []).

success_returns_upstream_message_with_ra_true_aa_false_and_orig_id(_Config) ->
    Stub = start_stub({respond_with, make_noerror_response()}),
    {ok, Port} = wait_for_stub_port(),
    Msg = make_query(),
    OrigId = Msg#dns_message.id,
    Result = registrator_dns_recursor:forward(Msg, [{"127.0.0.1", Port}]),
    stop_stub(Stub),
    {ok, Resp} = Result,
    ?DNS_RCODE_NOERROR = Resp#dns_message.rc,
    true = Resp#dns_message.ra,
    false = Resp#dns_message.aa,
    OrigId = Resp#dns_message.id,
    true = length(Resp#dns_message.answers) > 0.

success_preserves_upstream_servfail_rcode_passthrough(_Config) ->
    ServfailResp = #dns_message{qr = true, rc = ?DNS_RCODE_SERVFAIL, ra = false, aa = false},
    Stub = start_stub({respond_with, ServfailResp}),
    {ok, Port} = wait_for_stub_port(),
    Msg = make_query(),
    OrigId = Msg#dns_message.id,
    Result = registrator_dns_recursor:forward(Msg, [{"127.0.0.1", Port}]),
    stop_stub(Stub),
    {ok, Resp} = Result,
    ?DNS_RCODE_SERVFAIL = Resp#dns_message.rc,
    true = Resp#dns_message.ra,
    false = Resp#dns_message.aa,
    OrigId = Resp#dns_message.id.

success_preserves_upstream_nxdomain_passthrough(_Config) ->
    NxResp = #dns_message{qr = true, rc = ?DNS_RCODE_NXDOMAIN, ra = false, aa = false},
    Stub = start_stub({respond_with, NxResp}),
    {ok, Port} = wait_for_stub_port(),
    Msg = make_query(),
    OrigId = Msg#dns_message.id,
    Result = registrator_dns_recursor:forward(Msg, [{"127.0.0.1", Port}]),
    stop_stub(Stub),
    {ok, Resp} = Result,
    ?DNS_RCODE_NXDOMAIN = Resp#dns_message.rc,
    true = Resp#dns_message.ra,
    false = Resp#dns_message.aa,
    OrigId = Resp#dns_message.id.

success_preserves_upstream_ttls_verbatim(_Config) ->
    Answers600 = [#dns_rr{
        name = <<"upstream.example.">>,
        type = ?DNS_TYPE_A,
        ttl = 600,
        data = #dns_rrdata_a{ip = {5, 6, 7, 8}}
    }],
    RespMsg = (make_noerror_response())#dns_message{
        anc = length(Answers600),
        answers = Answers600
    },
    Stub = start_stub({respond_with, RespMsg}),
    {ok, Port} = wait_for_stub_port(),
    Msg = make_query(),
    Result = registrator_dns_recursor:forward(Msg, [{"127.0.0.1", Port}]),
    stop_stub(Stub),
    {ok, Resp} = Result,
    [Answer | _] = Resp#dns_message.answers,
    600 = Answer#dns_rr.ttl.

%% NOTE: This test takes ~2s for the first recursor timeout.
timeout_falls_through_to_next_recursor(Config) ->
    DropPort = proplists:get_value(stub1_port, Config),
    AnswerPort = proplists:get_value(stub2_port, Config),
    Msg = make_query(),
    OrigId = Msg#dns_message.id,
    Result = registrator_dns_recursor:forward(Msg, [{"127.0.0.1", DropPort}, {"127.0.0.1", AnswerPort}]),
    {ok, Resp} = Result,
    ?DNS_RCODE_NOERROR = Resp#dns_message.rc,
    true = Resp#dns_message.ra,
    false = Resp#dns_message.aa,
    OrigId = Resp#dns_message.id.

all_recursors_fail_returns_all_failed(_Config) ->
    Stub1 = start_stub({respond_with_bytes, <<0>>}),
    {ok, Port1} = wait_for_stub_port(),
    Stub2 = start_stub({respond_with_bytes, <<0>>}),
    {ok, Port2} = wait_for_stub_port(),
    Msg = make_query(),
    Result = registrator_dns_recursor:forward(Msg, [{"127.0.0.1", Port1}, {"127.0.0.1", Port2}]),
    stop_stub(Stub1),
    stop_stub(Stub2),
    {error, all_failed} = Result.

id_mismatch_treated_as_failure(Config) ->
    MismatchPort = proplists:get_value(stub1_port, Config),
    ValidPort = proplists:get_value(stub2_port, Config),
    Msg = make_query(),
    OrigId = Msg#dns_message.id,

    %% Single mismatch stub -> all_failed
    {error, all_failed} = registrator_dns_recursor:forward(Msg, [{"127.0.0.1", MismatchPort}]),

    %% Mismatch then valid -> success
    Result2 = registrator_dns_recursor:forward(Msg, [{"127.0.0.1", MismatchPort}, {"127.0.0.1", ValidPort}]),
    {ok, Resp} = Result2,
    OrigId = Resp#dns_message.id.

qr_false_treated_as_failure(_Config) ->
    QrFalseResp = #dns_message{qr = false, rc = ?DNS_RCODE_NOERROR},
    Stub = start_stub({respond_with, QrFalseResp}),
    {ok, Port} = wait_for_stub_port(),
    Msg = make_query(),
    Result = registrator_dns_recursor:forward(Msg, [{"127.0.0.1", Port}]),
    stop_stub(Stub),
    {error, all_failed} = Result.

socket_closed_on_success_and_failure(_Config) ->
    PortsBefore = length(erlang:ports()),

    %% Successful forward
    Stub1 = start_stub({respond_with, make_noerror_response()}),
    {ok, Port1} = wait_for_stub_port(),
    {ok, _} = registrator_dns_recursor:forward(make_query(), [{"127.0.0.1", Port1}]),
    stop_stub(Stub1),

    %% Failing forward (decode error)
    Stub2 = start_stub({respond_with_bytes, <<0>>}),
    {ok, Port2} = wait_for_stub_port(),
    {error, all_failed} = registrator_dns_recursor:forward(make_query(), [{"127.0.0.1", Port2}]),
    stop_stub(Stub2),

    PortsAfter = length(erlang:ports()),
    %% Allow some slack for the stub ports themselves, but forward's sockets should be closed
    true = (PortsAfter - PortsBefore) =< 4.
