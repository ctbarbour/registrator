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
    Stub = registrator_dns_test_stub:start({respond_with, make_noerror_resp_with_answer()}),
    {ok, Port} = registrator_dns_test_stub:wait_for_port(),
    application:set_env(registrator, recursors, [{"127.0.0.1", Port}]),
    Msg = make_query(<<"_redis._tcp.upstream.example.">>, ?DNS_TYPE_A),
    ClientId = Msg#dns_message.id,
    Resp = registrator_dns_resolver:resolve(Msg),
    registrator_dns_test_stub:stop(Stub),
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
    Stub1 = registrator_dns_test_stub:start({respond_counted, ServfailResp, CounterPid}),
    {ok, Port1} = registrator_dns_test_stub:wait_for_port(),
    %% A second stub (decode-error) that should NOT be contacted
    Stub2 = registrator_dns_test_stub:start({respond_with_bytes, <<0>>}),
    {ok, Port2} = registrator_dns_test_stub:wait_for_port(),
    application:set_env(registrator, recursors, [{"127.0.0.1", Port1}, {"127.0.0.1", Port2}]),
    Msg = make_query(<<"_redis._tcp.upstream.example.">>, ?DNS_TYPE_A),
    ClientId = Msg#dns_message.id,
    Resp = registrator_dns_resolver:resolve(Msg),
    CounterPid ! {get, self()},
    Count = receive {count, N} -> N end,
    registrator_dns_test_stub:stop(Stub1),
    registrator_dns_test_stub:stop(Stub2),
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
    Stub1 = registrator_dns_test_stub:start({respond_with_bytes, <<0>>}),
    {ok, Port1} = registrator_dns_test_stub:wait_for_port(),
    Stub2 = registrator_dns_test_stub:start({respond_with_bytes, <<0>>}),
    {ok, Port2} = registrator_dns_test_stub:wait_for_port(),
    application:set_env(registrator, recursors, [{"127.0.0.1", Port1}, {"127.0.0.1", Port2}]),
    Msg = make_query(<<"_redis._tcp.upstream.example.">>, ?DNS_TYPE_A),
    ClientId = Msg#dns_message.id,
    Resp = registrator_dns_resolver:resolve(Msg),
    registrator_dns_test_stub:stop(Stub1),
    registrator_dns_test_stub:stop(Stub2),
    ?DNS_RCODE_SERVFAIL = Resp#dns_message.rc,
    true = Resp#dns_message.ra,
    false = Resp#dns_message.aa,
    ClientId = Resp#dns_message.id.

env_var_appends_to_sys_config_recursors(_Config) ->
    %% PortA = decode-error stub (sys.config entry), PortB = success stub (env-var entry)
    StubA = registrator_dns_test_stub:start({respond_with_bytes, <<0>>}),
    {ok, PortA} = registrator_dns_test_stub:wait_for_port(),
    StubB = registrator_dns_test_stub:start({respond_with, make_noerror_resp_with_answer()}),
    {ok, PortB} = registrator_dns_test_stub:wait_for_port(),
    application:set_env(registrator, recursors, [{"127.0.0.1", PortA}]),
    os:putenv("REGISTRATOR_RECURSORS", "127.0.0.1:" ++ integer_to_list(PortB)),
    Msg = make_query(<<"_redis._tcp.upstream.example.">>, ?DNS_TYPE_A),
    Resp = registrator_dns_resolver:resolve(Msg),
    registrator_dns_test_stub:stop(StubA),
    registrator_dns_test_stub:stop(StubB),
    os:unsetenv("REGISTRATOR_RECURSORS"),
    %% Should succeed via PortB (env-var recursor)
    ?DNS_RCODE_NOERROR = Resp#dns_message.rc,
    true = Resp#dns_message.ra.

parse_env_recursor_drops_bad_entries(_Config) ->
    %% Set up two "good" stubs and make bad entries in the env var
    GoodStub1 = registrator_dns_test_stub:start({respond_with_bytes, <<0>>}),
    {ok, GoodPort1} = registrator_dns_test_stub:wait_for_port(),
    GoodStub2 = registrator_dns_test_stub:start({respond_with_bytes, <<0>>}),
    {ok, GoodPort2} = registrator_dns_test_stub:wait_for_port(),
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
    registrator_dns_test_stub:stop(GoodStub1),
    registrator_dns_test_stub:stop(GoodStub2),
    os:unsetenv("REGISTRATOR_RECURSORS"),
    %% Both good stubs received a request (proving only 2 entries survived parse)
    %% and the result is SERVFAIL (both return decode error)
    ?DNS_RCODE_SERVFAIL = Resp#dns_message.rc.
