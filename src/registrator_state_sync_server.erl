-module(registrator_state_sync_server).

%% Tiny TCP listener that serves anti-entropy pulls from peers.
%%
%% Wire protocol (one round-trip per connection):
%%   client -> server: term_to_binary({sync_request_v1, ClientActor})
%%   server -> client: term_to_binary({sync_response_v1, SyncPayload})
%%
%% `{packet, 4}` framing on the socket so each term is a self-contained frame.
%% No auth (Phase 1 -- trust the network); a future Phase 2 can HMAC the
%% handshake using the swim group key.

-behavior(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
	 terminate/2]).

-define(ACCEPT_TIMEOUT, 5000).
-define(RECV_TIMEOUT, 5000).

-record(state, {
	  listen_socket :: gen_tcp:socket(),
	  port          :: inet:port_number(),
	  ip            :: inet:ip_address()
	 }).

start_link(Ip, Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Ip, Port], []).

init([Ip, Port]) ->
    process_flag(trap_exit, true),
    Opts = [binary, {packet, 4}, {ip, Ip}, {reuseaddr, true},
	    {nodelay, true}, {active, false}],
    case gen_tcp:listen(Port, Opts) of
	{ok, Sock} ->
	    self() ! accept,
	    {ok, #state{listen_socket=Sock, port=Port, ip=Ip}};
	{error, Reason} ->
	    {stop, {listen_failed, Port, Reason}}
    end.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(accept, #state{listen_socket=Sock} = State) ->
    case gen_tcp:accept(Sock, ?ACCEPT_TIMEOUT) of
	{ok, Conn} ->
	    spawn(fun() -> handle_connection(Conn) end),
	    self() ! accept,
	    {noreply, State};
	{error, timeout} ->
	    self() ! accept,
	    {noreply, State};
	{error, closed} ->
	    {stop, normal, State};
	{error, Reason} ->
	    error_logger:warning_msg("state-sync accept error: ~p", [Reason]),
	    self() ! accept,
	    {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{listen_socket=Sock}) when is_port(Sock) ->
    catch gen_tcp:close(Sock),
    ok;
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ===================================================================
%%% Per-connection handler
%%% ===================================================================

handle_connection(Conn) ->
    try
	case gen_tcp:recv(Conn, 0, ?RECV_TIMEOUT) of
	    {ok, Frame} ->
		case safe_term(Frame) of
		    {sync_request_v1, _ClientActor} ->
			Payload = registrator_nodes:get_sync_payload(),
			Reply = term_to_binary({sync_response_v1, Payload}),
			gen_tcp:send(Conn, Reply);
		    _ ->
			ok
		end;
	    {error, _} ->
		ok
	end
    after
	gen_tcp:close(Conn)
    end.

safe_term(Bin) ->
    try binary_to_term(Bin, [safe])
    catch error:badarg -> {error, bad_term}
    end.
