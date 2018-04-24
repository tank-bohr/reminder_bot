-module(reminder_bot_commands).

-export([parse/1]).

-define(DEFAULT_TIME, {10, 0}).

parse(Str) ->
    Match = re:run(Str,
        "remind\s+me\s+(?<A>(on|at).+?)\s+(?<B>to.+)",
        [{capture, all_names, list}]),
    case Match of
        nomatch -> invalid_command;
        {match, [Time, Action]} -> response(Time, Action)
    end.

response(RawTime, Action) ->
    case parse_time(RawTime) of
        invalid_time -> invalid_command;
        Time -> #{action => Action, time => Time}
    end.

parse_time(RawTime) ->
    case legendary_goggles:parse(RawTime) of
        {{Y, M, D}, {H, M}} -> {{Y, M, D}, {H, M}};
        {Y, M, D} -> with_default_time(Y, M, D);
        {H, M} -> with_default_date(H, M);
        invalid -> invalid_time
    end.

with_default_time(Y, M, D) ->
    {{Y, M, D}, ?DEFAULT_TIME}.

with_default_date(H, M) ->
    {Today, _Time} = calendar:universal_time(),
    {Today, {H, M}}.
