-module(reminder_bot_commands_test).
-include_lib("eunit/include/eunit.hrl").

testee(Str) ->
    reminder_bot_commands:parse(Str).

parse_test() ->
    ?assertMatch(#{
        action := "to wish Linda happy birthday",
        time := {{_, 6, 1}, {10, 0}}
    }, testee("remind me on June 1st to wish Linda happy birthday")).

exact_time_test() ->
    ?assertMatch(#{
        action := "to go to gym",
        time := {{_, _, _}, {15, 0}}
    }, testee("remind me at 3pm to go to gym")).

invalid_command_test_() ->
    [
        ?_assertEqual(invalid_command, testee("remind me")),
        ?_assertEqual(invalid_command, testee("tell me")),
        ?_assertEqual(invalid_command, testee("o-lo-lo"))
    ].
