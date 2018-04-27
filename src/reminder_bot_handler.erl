-module(reminder_bot_handler).

-behaviour(cowboy_handler).
-export([
    init/2
]).

-type req()   :: cowboy_req:req().
-type state() :: any().

-spec init(req(), state()) -> {ok, req(), state()}.
init(Req0, State) ->
    {ok, Data, Req} = read_body(Req0),
    lager:debug(Data),
    {ok, cowboy_req:reply(204, Req), State}.

read_body(Req) ->
    read_body(Req, <<>>).

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} ->
            {ok, <<Acc/binary, Data/binary>>, Req};
        {more, Data, Req} ->
            read_body(Req, <<Acc/binary, Data/binary>>)
    end.
