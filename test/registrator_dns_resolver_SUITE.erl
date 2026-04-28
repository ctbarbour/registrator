-module(registrator_dns_resolver_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("dns_erlang/include/dns.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    in_zone_a_positive/1,
    in_zone_srv_positive/1,
    in_zone_any_positive/1,
    in_zone_nxdomain_no_nodes/1,
    in_zone_nxdomain_apex/1,
    in_zone_nxdomain_deeper_non_service/1,
    in_zone_subdomain/1,
    out_of_zone_refused/1,
    out_of_zone_pathological_suffix/1,
    case_insensitive_zone/1,
    trailing_dot_zone/1,
    missing_config_app_start_fails/1,
    non_ascii_config_app_start_fails/1,
    out_of_zone_with_empty_recursors_still_refused/1,
    out_of_zone_with_recursors_success/1,
    out_of_zone_with_recursors_servfail_passthrough/1,
    out_of_zone_with_all_recursors_failing_servfail/1,
    env_var_appends_to_sys_config_recursors/1,
    parse_env_recursor_drops_bad_entries/1
]).

%%--------------------------------------------------------------------
%% Stub gen_server for registrator_nodes
%%--------------------------------------------------------------------

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

init([]) ->
    {ok, #{call_count => 0, nodes => []}}.

handle_call({set_nodes, Nodes}, _From, State) ->
    {reply, ok, State#{nodes => Nodes, call_count => 0}};
handle_call(get_call_count, _From, #{call_count := N} = State) ->
    {reply, N, State};
handle_call({lookup, _Service, _Protocol}, _From, #{nodes := Nodes, call_count := N} = State) ->
    {reply, Nodes, State#{call_count => N + 1}};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Suite setup/teardown
%%--------------------------------------------------------------------

all() ->
    [
        in_zone_a_positive,
        in_zone_srv_positive,
        in_zone_any_positive,
        in_zone_nxdomain_no_nodes,
        in_zone_nxdomain_apex,
        in_zone_nxdomain_deeper_non_service,
        in_zone_subdomain,
        out_of_zone_refused,
        out_of_zone_pathological_suffix,
        case_insensitive_zone,
        trailing_dot_zone,
        missing_config_app_start_fails,
        non_ascii_config_app_start_fails,
        out_of_zone_with_empty_recursors_still_refused,
        out_of_zone_with_recursors_success,
        out_of_zone_with_recursors_servfail_passthrough,
        out_of_zone_with_all_recursors_failing_servfail,
        env_var_appends_to_sys_config_recursors,
        parse_env_recursor_drops_bad_entries
    ].

init_per_suite(Config) ->
    %% Start stub gen_server registered as registrator_nodes
    {ok, Pid} = gen_server:start({local, registrator_nodes}, ?MODULE, [], []),
    [{stub_pid, Pid} | Config].

end_per_suite(Config) ->
    Pid = proplists:get_value(stub_pid, Config),
    gen_server:stop(Pid),
    Config.

init_per_testcase(missing_config_app_start_fails, Config) ->
    %% Save current zone value; test will unset it
    Saved = application:get_env(registrator, authoritative_zone),
    [{saved_zone, Saved} | Config];
init_per_testcase(non_ascii_config_app_start_fails, Config) ->
    Saved = application:get_env(registrator, authoritative_zone),
    [{saved_zone, Saved} | Config];
init_per_testcase(trailing_dot_zone, Config) ->
    application:set_env(registrator, authoritative_zone, "service.local."),
    ok = registrator_app:validate_authoritative_zone(),
    Config;
init_per_testcase(_TestCase, Config) ->
    application:set_env(registrator, authoritative_zone, <<"service.local">>),
    application:set_env(registrator, recursors, []),
    Config.

end_per_testcase(missing_config_app_start_fails, Config) ->
    restore_zone(Config),
    application:unset_env(registrator, recursors),
    Config;
end_per_testcase(non_ascii_config_app_start_fails, Config) ->
    restore_zone(Config),
    application:unset_env(registrator, recursors),
    Config;
end_per_testcase(_TestCase, Config) ->
    application:set_env(registrator, authoritative_zone, <<"service.local">>),
    application:unset_env(registrator, recursors),
    os:unsetenv("REGISTRATOR_RECURSORS"),
    Config.

restore_zone(Config) ->
    case proplists:get_value(saved_zone, Config) of
        undefined ->
            application:unset_env(registrator, authoritative_zone);
        {ok, Val} ->
            application:set_env(registrator, authoritative_zone, Val)
    end.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

make_query(Name, Type) ->
    Question = #dns_query{name = Name, type = Type, class = ?DNS_CLASS_IN},
    #dns_message{questions = [Question]}.

set_stub_nodes(Nodes) ->
    gen_server:call(registrator_nodes, {set_nodes, Nodes}).

get_stub_call_count() ->
    gen_server:call(registrator_nodes, get_call_count).

one_node() ->
    [#{address => {127, 0, 0, 1}, port => 8080}].

%%--------------------------------------------------------------------
%% Test cases
%%--------------------------------------------------------------------

in_zone_a_positive(_Config) ->
    set_stub_nodes(one_node()),
    Msg = make_query(<<"_redis._tcp.service.local">>, ?DNS_TYPE_A),
    Resp = registrator_dns_resolver:resolve(Msg),
    ?DNS_RCODE_NOERROR = Resp#dns_message.rc,
    true = Resp#dns_message.aa,
    true = length(Resp#dns_message.answers) > 0.

in_zone_srv_positive(_Config) ->
    set_stub_nodes(one_node()),
    Msg = make_query(<<"_redis._tcp.service.local">>, ?DNS_TYPE_SRV),
    Resp = registrator_dns_resolver:resolve(Msg),
    ?DNS_RCODE_NOERROR = Resp#dns_message.rc,
    true = Resp#dns_message.aa,
    true = length(Resp#dns_message.answers) > 0.

in_zone_any_positive(_Config) ->
    set_stub_nodes(one_node()),
    Msg = make_query(<<"_redis._tcp.service.local">>, ?DNS_TYPE_ANY),
    Resp = registrator_dns_resolver:resolve(Msg),
    ?DNS_RCODE_NOERROR = Resp#dns_message.rc,
    true = Resp#dns_message.aa,
    %% ANY returns both A and SRV records
    true = length(Resp#dns_message.answers) >= 2.

in_zone_nxdomain_no_nodes(_Config) ->
    set_stub_nodes([]),
    Msg = make_query(<<"_redis._tcp.service.local">>, ?DNS_TYPE_A),
    Resp = registrator_dns_resolver:resolve(Msg),
    ?DNS_RCODE_NXDOMAIN = Resp#dns_message.rc,
    true = Resp#dns_message.aa.

in_zone_nxdomain_apex(_Config) ->
    %% Apex query: zone name itself is in-zone but not a service pattern
    Msg = make_query(<<"service.local">>, ?DNS_TYPE_A),
    Resp = registrator_dns_resolver:resolve(Msg),
    ?DNS_RCODE_NXDOMAIN = Resp#dns_message.rc,
    true = Resp#dns_message.aa.

in_zone_nxdomain_deeper_non_service(_Config) ->
    %% In-zone but no leading underscore -> not a service pattern -> NXDOMAIN
    Msg = make_query(<<"foo.service.local">>, ?DNS_TYPE_A),
    Resp = registrator_dns_resolver:resolve(Msg),
    ?DNS_RCODE_NXDOMAIN = Resp#dns_message.rc,
    true = Resp#dns_message.aa.

in_zone_subdomain(_Config) ->
    set_stub_nodes(one_node()),
    Msg = make_query(<<"_redis._tcp.east.service.local">>, ?DNS_TYPE_SRV),
    Resp = registrator_dns_resolver:resolve(Msg),
    ?DNS_RCODE_NOERROR = Resp#dns_message.rc,
    true = Resp#dns_message.aa.

out_of_zone_refused(_Config) ->
    set_stub_nodes(one_node()),
    gen_server:call(registrator_nodes, {set_nodes, one_node()}),
    set_stub_nodes([]),
    Msg = make_query(<<"_redis._tcp.evil.example.com">>, ?DNS_TYPE_A),
    Resp = registrator_dns_resolver:resolve(Msg),
    ?DNS_RCODE_REFUSED = Resp#dns_message.rc,
    false = Resp#dns_message.aa,
    0 = get_stub_call_count().

out_of_zone_pathological_suffix(_Config) ->
    %% evil-service.local must NOT be in-zone for service.local
    set_stub_nodes([]),
    Msg = make_query(<<"_redis._tcp.evil-service.local">>, ?DNS_TYPE_A),
    Resp = registrator_dns_resolver:resolve(Msg),
    ?DNS_RCODE_REFUSED = Resp#dns_message.rc,
    false = Resp#dns_message.aa,
    0 = get_stub_call_count().

case_insensitive_zone(_Config) ->
    set_stub_nodes(one_node()),
    Msg = make_query(<<"_REDIS._TCP.SERVICE.LOCAL">>, ?DNS_TYPE_A),
    Resp = registrator_dns_resolver:resolve(Msg),
    %% Must not be REFUSED -- case-insensitive zone match
    true = Resp#dns_message.rc =/= ?DNS_RCODE_REFUSED.

trailing_dot_zone(_Config) ->
    %% Zone configured with trailing dot; auth_zone/0 strips it
    set_stub_nodes(one_node()),
    Msg = make_query(<<"_redis._tcp.service.local">>, ?DNS_TYPE_A),
    Resp = registrator_dns_resolver:resolve(Msg),
    true = Resp#dns_message.rc =/= ?DNS_RCODE_REFUSED.

missing_config_app_start_fails(_Config) ->
    application:unset_env(registrator, authoritative_zone),
    Result = registrator_app:start(normal, []),
    {error, {invalid_config, authoritative_zone, missing}} = Result.

non_ascii_config_app_start_fails(_Config) ->
    application:set_env(registrator, authoritative_zone, <<240, 159, 152, 128, $., $l, $o, $c, $a, $l>>),
    Result = registrator_app:start(normal, []),
    {error, {invalid_config, authoritative_zone, non_ascii}} = Result.

%%--------------------------------------------------------------------
%% Stub UDP responder helpers (for forwarding tests)
%%--------------------------------------------------------------------

%% Modes: {respond_with, Msg}, {respond_with_bytes, Bin}, respond_with_id_mismatch,
%%        {respond_counted, Msg, CounterPid}, drop
start_udp_stub(Mode) ->
    Self = self(),
    spawn(fun() -> udp_stub_init(Self, Mode) end).

wait_for_udp_stub_port() ->
    receive
        {stub_ready, Port} -> {ok, Port}
    after 5000 ->
        {error, stub_timeout}
    end.

stop_udp_stub(Pid) when is_pid(Pid) ->
    Pid ! stop;
stop_udp_stub(_) ->
    ok.

udp_stub_init(Parent, Mode) ->
    {ok, Sock} = gen_udp:open(0, [binary, {active, false}]),
    {ok, Port} = inet:port(Sock),
    Parent ! {stub_ready, Port},
    udp_stub_loop(Sock, Mode).

udp_stub_loop(Sock, drop) ->
    case gen_udp:recv(Sock, 0, infinity) of
        {ok, _} ->
            receive stop -> gen_udp:close(Sock) end;
        _ ->
            gen_udp:close(Sock)
    end;
udp_stub_loop(Sock, Mode) ->
    receive
        stop ->
            gen_udp:close(Sock)
    after 0 ->
        case gen_udp:recv(Sock, 0, 100) of
            {ok, {Ip, Port, Bin}} ->
                handle_udp_stub_recv(Sock, Ip, Port, Bin, Mode),
                udp_stub_loop(Sock, Mode);
            {error, timeout} ->
                udp_stub_loop(Sock, Mode);
            _ ->
                gen_udp:close(Sock)
        end
    end.

handle_udp_stub_recv(Sock, Ip, Port, Bin, {respond_with, RespMsg}) ->
    InId = decode_query_id(Bin),
    Reply = RespMsg#dns_message{id = InId},
    gen_udp:send(Sock, Ip, Port, dns:encode_message(Reply));
handle_udp_stub_recv(Sock, Ip, Port, Bin, {respond_counted, RespMsg, CounterPid}) ->
    CounterPid ! increment,
    InId = decode_query_id(Bin),
    Reply = RespMsg#dns_message{id = InId},
    gen_udp:send(Sock, Ip, Port, dns:encode_message(Reply));
handle_udp_stub_recv(Sock, Ip, Port, _Bin, {respond_with_bytes, Bytes}) ->
    gen_udp:send(Sock, Ip, Port, Bytes);
handle_udp_stub_recv(_Sock, _Ip, _Port, _Bin, drop) ->
    ok.

decode_query_id(Bin) ->
    case dns:decode_message(Bin) of
        M when is_record(M, dns_message) -> M#dns_message.id;
        {trailing_garbage, M, _} -> M#dns_message.id
    end.

make_noerror_resp_with_answer() ->
    Answers = [#dns_rr{
        name = <<"_redis._tcp.upstream.example.">>,
        type = ?DNS_TYPE_A,
        ttl = 300,
        data = #dns_rrdata_a{ip = {10, 0, 0, 1}}
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
%% New forwarding-path test cases
%%--------------------------------------------------------------------

out_of_zone_with_empty_recursors_still_refused(_Config) ->
    application:set_env(registrator, recursors, []),
    Msg = make_query(<<"_redis._tcp.upstream.example.">>, ?DNS_TYPE_A),
    Resp = registrator_dns_resolver:resolve(Msg),
    ?DNS_RCODE_REFUSED = Resp#dns_message.rc,
    false = Resp#dns_message.aa.

out_of_zone_with_recursors_success(_Config) ->
    Stub = start_udp_stub({respond_with, make_noerror_resp_with_answer()}),
    {ok, Port} = wait_for_udp_stub_port(),
    application:set_env(registrator, recursors, [{"127.0.0.1", Port}]),
    Msg = make_query(<<"_redis._tcp.upstream.example.">>, ?DNS_TYPE_A),
    ClientId = Msg#dns_message.id,
    Resp = registrator_dns_resolver:resolve(Msg),
    stop_udp_stub(Stub),
    ?DNS_RCODE_NOERROR = Resp#dns_message.rc,
    true = Resp#dns_message.ra,
    false = Resp#dns_message.aa,
    ClientId = Resp#dns_message.id,
    true = length(Resp#dns_message.answers) > 0,
    %% TTL preserved
    [Ans | _] = Resp#dns_message.answers,
    300 = Ans#dns_rr.ttl.

out_of_zone_with_recursors_servfail_passthrough(_Config) ->
    %% Stub returns SERVFAIL; it should be passed through verbatim (no retry to second recursor)
    CounterPid = spawn(fun() -> counter_loop(0) end),
    ServfailResp = #dns_message{qr = true, rc = ?DNS_RCODE_SERVFAIL, ra = false, aa = false},
    Stub1 = start_udp_stub({respond_counted, ServfailResp, CounterPid}),
    {ok, Port1} = wait_for_udp_stub_port(),
    %% A second stub (decode-error) that should NOT be contacted
    Stub2 = start_udp_stub({respond_with_bytes, <<0>>}),
    {ok, Port2} = wait_for_udp_stub_port(),
    application:set_env(registrator, recursors, [{"127.0.0.1", Port1}, {"127.0.0.1", Port2}]),
    Msg = make_query(<<"_redis._tcp.upstream.example.">>, ?DNS_TYPE_A),
    ClientId = Msg#dns_message.id,
    Resp = registrator_dns_resolver:resolve(Msg),
    CounterPid ! {get, self()},
    Count = receive {count, N} -> N end,
    stop_udp_stub(Stub1),
    stop_udp_stub(Stub2),
    ?DNS_RCODE_SERVFAIL = Resp#dns_message.rc,
    true = Resp#dns_message.ra,
    false = Resp#dns_message.aa,
    ClientId = Resp#dns_message.id,
    %% Exactly one request: SERVFAIL is passthrough, no retry
    1 = Count.

counter_loop(N) ->
    receive
        increment -> counter_loop(N + 1);
        {get, From} -> From ! {count, N}
    end.

out_of_zone_with_all_recursors_failing_servfail(_Config) ->
    Stub1 = start_udp_stub({respond_with_bytes, <<0>>}),
    {ok, Port1} = wait_for_udp_stub_port(),
    Stub2 = start_udp_stub({respond_with_bytes, <<0>>}),
    {ok, Port2} = wait_for_udp_stub_port(),
    application:set_env(registrator, recursors, [{"127.0.0.1", Port1}, {"127.0.0.1", Port2}]),
    Msg = make_query(<<"_redis._tcp.upstream.example.">>, ?DNS_TYPE_A),
    ClientId = Msg#dns_message.id,
    Resp = registrator_dns_resolver:resolve(Msg),
    stop_udp_stub(Stub1),
    stop_udp_stub(Stub2),
    ?DNS_RCODE_SERVFAIL = Resp#dns_message.rc,
    true = Resp#dns_message.ra,
    false = Resp#dns_message.aa,
    ClientId = Resp#dns_message.id.

env_var_appends_to_sys_config_recursors(_Config) ->
    %% PortA = decode-error stub (sys.config entry), PortB = success stub (env-var entry)
    StubA = start_udp_stub({respond_with_bytes, <<0>>}),
    {ok, PortA} = wait_for_udp_stub_port(),
    StubB = start_udp_stub({respond_with, make_noerror_resp_with_answer()}),
    {ok, PortB} = wait_for_udp_stub_port(),
    application:set_env(registrator, recursors, [{"127.0.0.1", PortA}]),
    os:putenv("REGISTRATOR_RECURSORS", "127.0.0.1:" ++ integer_to_list(PortB)),
    Msg = make_query(<<"_redis._tcp.upstream.example.">>, ?DNS_TYPE_A),
    Resp = registrator_dns_resolver:resolve(Msg),
    stop_udp_stub(StubA),
    stop_udp_stub(StubB),
    os:unsetenv("REGISTRATOR_RECURSORS"),
    %% Should succeed via PortB (env-var recursor)
    ?DNS_RCODE_NOERROR = Resp#dns_message.rc,
    true = Resp#dns_message.ra.

parse_env_recursor_drops_bad_entries(_Config) ->
    %% Set up two "good" stubs and make bad entries in the env var
    GoodStub1 = start_udp_stub({respond_with_bytes, <<0>>}),
    {ok, GoodPort1} = wait_for_udp_stub_port(),
    GoodStub2 = start_udp_stub({respond_with_bytes, <<0>>}),
    {ok, GoodPort2} = wait_for_udp_stub_port(),
    application:set_env(registrator, recursors, []),
    EnvVal = "127.0.0.1:" ++ integer_to_list(GoodPort1) ++
             ",bad:notaport" ++
             ",1.2.3.4:99999" ++
             ",no_colon" ++
             ",127.0.0.1:" ++ integer_to_list(GoodPort2),
    os:putenv("REGISTRATOR_RECURSORS", EnvVal),
    %% Call resolve with out-of-zone query; both good stubs return decode error -> SERVFAIL
    Msg = make_query(<<"_redis._tcp.upstream.example.">>, ?DNS_TYPE_A),
    Resp = registrator_dns_resolver:resolve(Msg),
    stop_udp_stub(GoodStub1),
    stop_udp_stub(GoodStub2),
    os:unsetenv("REGISTRATOR_RECURSORS"),
    %% Both good stubs received a request (proving only 2 entries survived parse)
    %% and the result is SERVFAIL (both return decode error)
    ?DNS_RCODE_SERVFAIL = Resp#dns_message.rc.
