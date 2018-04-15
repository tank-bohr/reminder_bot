-module(reminder_bot_json_test).
-include_lib("eunit/include/eunit.hrl").

encode_test_() ->
    [
        ?_assertEqual(<<"{}">>, reminder_bot_json:encode(#{})),
        ?_assertEqual(<<"{\"foo\":\"Bar\"}">>, reminder_bot_json:encode(#{foo => <<"Bar">>}))
    ].

decode_test() ->
    ?assertEqual(#{foo => <<"Bar">>}, reminder_bot_json:decode(<<"{\"foo\": \"Bar\"}">>)).
