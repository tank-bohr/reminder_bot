-module(reminder_bot).
-export([
    start/0,
    token/0,
    port/0
]).

-define(APPLICATION, reminder_bot).

-spec start() -> ok.
start() ->
    {ok, Started} = application:ensure_all_started(?APPLICATION),
    lager:debug("Applications started: ~p", [Started]).

-spec token() -> string() | no_token.
token() ->
    application:get_env(?APPLICATION, token, no_token).

-spec port() -> integer().
port() ->
    application:get_env(?APPLICATION, port, 8080).
