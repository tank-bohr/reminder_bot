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
    Update = reminder_bot_json:decode(Data),
    case reminder_bot_telegram:peel_update(Update) of
        #{text := Command, user_id := UserId} ->
            process_command(Command, UserId);
        _ ->
            lager:error("Invalid update: [~p]", [Update])
    end,
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

process_command(Command, UserId) ->
    case reminder_bot_commands:parse(Command) of
        invalid_command ->
            invalid_command(Command);
        Event ->
            add_event(Event, UserId)
    end.

invalid_command(Command) ->
    lager:info("Invalid command: ~p", [Command]).

add_event(Event, UserId) ->
    ok = reminder_bot_scheduler:add_event(maps:put(user_id, UserId, Event)).
