-module(reminder_bot_telegram_test).
-include_lib("eunit/include/eunit.hrl").

-define(TESTEE, reminder_bot_telegram).

peel_update_test() ->
    Update = #{
        update_id => 548147569,
        message => #{
            text => <<"/start">>,
            message_id => 11,
            from => #{
                username => <<"john_doe">>,
                first_name => <<"John">>,
                last_name => <<"Doe">>,
                language_code => <<"en-RU">>,
                is_bot => false,
                id => 185375011
            },
            entities => [
                #{
                    type => <<"bot_command">>,
                    offset => 0,
                    length => 7
                }
            ],
            date => 1524837671,
            chat => #{
                username => <<"john_doe">>,
                type => <<"private">>,
                first_name => <<"John">>,
                last_name => <<"Doe">>,
                id => 185375011
            }
        }
    },
    Peeled = ?TESTEE:peel_update(Update),
    ?assertEqual(#{text => <<"/start">>, user_id => 185375011}, Peeled).
