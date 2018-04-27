-module(reminder_bot_handler).

-behaviour(cowboy_handler).
-export([
    init/2
]).

-type req()   :: cowboy_req:req().
-type state() :: any().

-spec init(req(), state()) -> {ok, req(), state()}.
init(Req0, State) ->
    Response = reminder_bot_json:encode(#{
        result => ok
    }),
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, Response, Req0),
    {ok, Req, State}.
