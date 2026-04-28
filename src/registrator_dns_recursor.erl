-module(registrator_dns_recursor).

-include_lib("dns_erlang/include/dns.hrl").

-export([forward/2]).

-define(RECURSOR_TIMEOUT_MS, 2000).

-spec forward(#dns_message{}, [{string(), 1..65535}]) ->
    {ok, #dns_message{}} | {error, no_recursors | all_failed}.
forward(_Message, []) ->
    {error, no_recursors};
forward(Message, Recursors) ->
    OrigId = Message#dns_message.id,
    {UpstreamId, Wire} = make_upstream_query(Message),
    try_recursors(OrigId, UpstreamId, Wire, Recursors).

make_upstream_query(Message) ->
    UpstreamId = dns:random_id(),
    Questions = Message#dns_message.questions,
    {FirstQuestion, QC} = case Questions of
        [] -> {[], 0};
        [Q | _] -> {[Q], 1}
    end,
    UpstreamMsg = Message#dns_message{id = UpstreamId, questions = FirstQuestion, qc = QC},
    Wire = dns:encode_message(UpstreamMsg),
    {UpstreamId, Wire}.

try_recursors(_OrigId, _UpstreamId, _Wire, []) ->
    {error, all_failed};
try_recursors(OrigId, UpstreamId, Wire, [Rec | Rest]) ->
    case do_forward(OrigId, UpstreamId, Wire, Rec) of
        {ok, Msg} -> {ok, Msg};
        {error, _} -> try_recursors(OrigId, UpstreamId, Wire, Rest)
    end.

do_forward(OrigId, UpstreamId, Wire, {Host, Port}) ->
    {ok, Sock} = gen_udp:open(0, [binary, {active, false}, {recbuf, 4096}]),
    try
        case gen_udp:send(Sock, Host, Port, Wire) of
            {error, Reason} ->
                {error, {send_failed, Reason}};
            ok ->
                case gen_udp:recv(Sock, 0, ?RECURSOR_TIMEOUT_MS) of
                    {error, timeout} ->
                        {error, timeout};
                    {error, Reason} ->
                        {error, {recv_failed, Reason}};
                    {ok, {_Ip, _Port, Bin}} ->
                        case dns:decode_message(Bin) of
                            UpstreamMsg when is_record(UpstreamMsg, dns_message) ->
                                validate_and_build(OrigId, UpstreamId, UpstreamMsg);
                            {trailing_garbage, UpstreamMsg, _} when is_record(UpstreamMsg, dns_message) ->
                                validate_and_build(OrigId, UpstreamId, UpstreamMsg);
                            _ ->
                                {error, decode_error}
                        end
                end
        end
    after
        gen_udp:close(Sock)
    end.

validate_and_build(OrigId, UpstreamId, UpstreamMsg) ->
    case UpstreamMsg#dns_message.id =:= UpstreamId of
        false ->
            {error, id_mismatch};
        true ->
            case UpstreamMsg#dns_message.qr of
                false ->
                    {error, not_a_response};
                true ->
                    {ok, UpstreamMsg#dns_message{id = OrigId, ra = true, aa = false}}
            end
    end.
