-module(reminder_bot).
-export([
    start/0,
    token/0,
    port/0
]).

-define(APPLICATION, reminder_bot).
-define(DEFAULT_PORT, 8080).

-spec start() -> ok.
start() ->
    {ok, Started} = application:ensure_all_started(?APPLICATION),
    lager:debug("Applications started: ~p", [Started]).

-spec token() -> string() | no_token.
token() ->
    application:get_env(?APPLICATION, token, no_token).

-spec port() -> integer().
port() ->
    case application:get_env(?APPLICATION, port, ?DEFAULT_PORT) of
        Port when is_list(Port) ->
            list_to_integer(Port);
        Port when is_integer(Port) ->
            Port;
        Port ->
            lager:error("Port must be string or integer: [~p].", [Port]),
            ?DEFAULT_PORT
        end.
