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
    non_ascii_config_app_start_fails/1
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
        non_ascii_config_app_start_fails
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
    Config.

end_per_testcase(missing_config_app_start_fails, Config) ->
    restore_zone(Config),
    Config;
end_per_testcase(non_ascii_config_app_start_fails, Config) ->
    restore_zone(Config),
    Config;
end_per_testcase(_TestCase, Config) ->
    application:set_env(registrator, authoritative_zone, <<"service.local">>),
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
