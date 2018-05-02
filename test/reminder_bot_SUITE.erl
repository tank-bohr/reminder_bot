-module(reminder_bot_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(APP, reminder_bot).
-define(PORT, 4321).
-define(USER_ID, 1111111).
-define(ACTION, "to eat my pills").

-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    add_reminder_test/1,
    fire_reminder_test/1
]).

all() ->
    [
        {group, end_to_end}
    ].

groups() ->
    [
        {end_to_end, [shuffle], [
            add_reminder_test,
            fire_reminder_test
        ]}
    ].

init_per_suite(Config) ->
    ok = application:set_env(?APP, port, ?PORT),
    ok = application:set_env(?APP, token, <<"TOKEN_TOKEN">>),
    ok = ?APP:start(),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(?APP).

init_per_group(end_to_end, Config) ->
    Config.

end_per_group(end_to_end, _Config) ->
    ok.

init_per_testcase(add_reminder_test, Config) ->
    ok = gen_server:call(reminder_bot_scheduler, {set_timer, 300000}),
    Config;
init_per_testcase(fire_reminder_test, Config) ->
    ok = gen_server:call(reminder_bot_scheduler, {set_timer, 10}),
    Config;
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(add_reminder_test, _Config) ->
    gen_server:call(reminder_bot_scheduler, {dry_run, false});
end_per_testcase(_TestCase, _Config) ->
    ok.

add_reminder_test(_Config) ->
    Time = current_time(),
    {ok, {StatusCode, _}} = httpc:request(post, {url(), [], "application/json", json(Time)},
        [], [{full_result, false}]),
    ?assertEqual(204, StatusCode),
    [
        {_, {_, Time}, Action, UserId}
    ] = ets:tab2list(reminder_bot_scheduler),
    ?assertEqual(?ACTION, Action),
    ?assertEqual(?USER_ID, UserId).

fire_reminder_test(_Config) ->
    Server = webserver:start(),
    {{Y, M, D}, {H, Min, _}} = calendar:universal_time(),
    ok = reminder_bot_scheduler:add_event(#{
        time =>  {{Y, M, D}, {H, Min}},
        action => ?ACTION,
        user_id => ?USER_ID
    }),
    ok = receive
        {webserver, {_Headers, Body}} ->
            #{
                chat_id := ChatID,
                text := Action
            } = reminder_bot_json:decode(Body),
            ?assertEqual(?USER_ID, ChatID),
            ?assertEqual(?ACTION, binary_to_list(Action)),
            ok
        after 5000 ->
            timeout
    end,
    ok = webserver:stop(Server).

url() ->
    "http://localhost:" ++ integer_to_list(?PORT) ++ "/webhook".

current_time() ->
    {_, {H, M, _}} = calendar:universal_time(),
    {H, M}.

json({H, M}) ->
    Time = integer_to_list(H) ++ ":" ++ integer_to_list(M),
    reminder_bot_json:encode(#{
        message => #{
            text => iolist_to_binary([<<"remind me at ">>, Time, " ", ?ACTION]),
            from => #{id => ?USER_ID}
        }
    }).
