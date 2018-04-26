-module(reminder_bot_scheduler).
-export([
    start_link/0,
    add_event/1
]).

-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).
-define(POLL_MILLIS, 60000).

-record(entry, {time, action}).

-type entry() :: #entry{}.
-type date() :: calendar:date().
-type hour() :: 0..23.
-type minute() :: 0..59.
-type time() :: {date(), {hour(), minute()}}.
-type action() :: nonempty_string().
-type event() :: #{
    time := time(),
    action := action()
}.
-type state() :: #{
    timer => timer:tref()
}.

-spec start_link() -> {ok, pid()}.
start_link() ->
    Args = [],
    Options = [],
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, Options).

-spec add_event(entry()) -> term().
add_event(Event) ->
    gen_server:call(?SERVER, {add_event, Event}).

-spec init([]) -> {ok, state()}.
init([]) ->
    create_table(),
    {ok, Timer} = timer:send_interval(?POLL_MILLIS, check),
    {ok, #{timer => Timer}}.

-spec handle_call({add_event, event()}, {pid(), _}, state()) -> {reply, ok, state()} | {reply, {error, unknown_call}, state()}.
handle_call({add_event, Event}, _From, State) ->
    #{time := Time, action := Action} = Event,
    Entry = #entry{time = Time, action = Action},
    true = ets:insert(?TAB, Entry),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(check, state()) -> {noreply, state()}.
handle_info(check, State) ->
    check(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

create_table() ->
    ets:new(?TAB, [
        bag,
        public,
        named_table,
        {keypos, #entry.time}
    ]).

check() ->
    {Date, {H, M, _}} = calendar:universal_time(),
    Key = {Date, {H, M}},
    case ets:lookup(?TAB, Key) of
        [] ->
            ok;
        Objects ->
            lists:foreach(fun(#entry{action = Action}) ->
                lager:debug(Action),
                ets:delete(?TAB, Key)
            end, Objects)
    end.
