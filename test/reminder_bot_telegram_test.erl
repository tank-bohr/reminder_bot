-module(reminder_bot_telegram_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("reminder_bot_telegram.hrl").

when_token_not_set_test() ->
    ?assertEqual(no_token, reminder_bot_telegram:token()).

when_token_is_set_test_() ->
    {
        foreach,
        fun setup_token/0,
        fun clear_token/1,
        [fun when_token_is_set/1]
    }.

setup_token() ->
    Token = "123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11",
    os:putenv("TOKEN", Token).

clear_token(_SetupData) ->
    os:unsetenv("TOKEN").

when_token_is_set(_SetupData) ->
    [
        ?_assertEqual(
            {ok, "123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11"},
            reminder_bot_telegram:token()),
        ?_assertEqual(
            "https://api.telegram.org/bot123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11/getMe",
            reminder_bot_telegram:url("getMe", ?TELEGRAM_BASE_URL)),
        ?_assertEqual(
            "https://api.telegram.org/bot123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11/getUpdates",
            reminder_bot_telegram:url("getUpdates", ?TELEGRAM_BASE_URL))
    ].
