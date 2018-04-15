-module(reminder_bot).
-export([start/0]).

-define(APPLICATION, reminder_bot).

start() ->
    {ok, Started} = application:ensure_all_started(?APPLICATION),
    lager:debug("Applications started: ~p", [Started]).
