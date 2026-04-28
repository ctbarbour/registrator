-module(registrator_app).

-behavior(application).

-export([start/2, stop/1, validate_authoritative_zone/0]).

start(_StartType, _StartArgs) ->
    case validate_authoritative_zone() of
        ok ->
            registrator_sup:start_link();
        {error, Reason} ->
            {error, {invalid_config, authoritative_zone, Reason}}
    end.

stop(_State) ->
    ok.

validate_authoritative_zone() ->
    case application:get_env(registrator, authoritative_zone) of
        undefined ->
            {error, missing};
        {ok, Value} when is_list(Value); is_binary(Value) ->
            Raw = iolist_to_binary(Value),
            case Raw of
                <<>> ->
                    {error, empty};
                _ ->
                    Stripped = case binary:last(Raw) of
                        $. -> binary:part(Raw, 0, byte_size(Raw) - 1);
                        _  -> Raw
                    end,
                    case Stripped of
                        <<>> ->
                            {error, empty};
                        _ ->
                            Canonical = dns_domain:to_lower(Stripped),
                            case check_ascii(Canonical) of
                                ok ->
                                    application:set_env(registrator, authoritative_zone, Canonical),
                                    ok;
                                Error ->
                                    Error
                            end
                    end
            end;
        {ok, _} ->
            {error, not_text}
    end.

check_ascii(Bin) ->
    case re:run(Bin, <<"^[a-z0-9\\-.]+$">>, [caseless]) of
        {match, _} -> ok;
        nomatch     -> {error, non_ascii}
    end.
