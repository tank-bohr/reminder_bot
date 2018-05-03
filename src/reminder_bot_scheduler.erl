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
    terminate/2,
    code_change/3
]).

-define(POLL_MILLIS, 60000).
-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).

-record(entry, {time, action, user_id}).

-type date() :: calendar:date().
-type hour() :: 0..23.
-type minute() :: 0..59.
-type time() :: {date(), {hour(), minute()}}.
-type action() :: nonempty_string().
-type event() :: #{
    time := time(),
    action := action(),
    user_id := integer()
}.
-type call() :: {add_event, event()} |
                {set_timer, integer()}.
-type state() :: #{
    timer := timer:tref()
}.

-spec start_link() -> {ok, pid()}.
start_link() ->
    Args = [],
    Options = [],
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, Options).

-spec add_event(event()) -> term().
add_event(Event) ->
    gen_server:call(?SERVER, {add_event, Event}).

-spec init([]) -> {ok, state()}.
init([]) ->
    create_table(),
    Timer = set_timer(),
    {ok, #{timer => Timer}}.

-spec handle_call(call(), {pid(), _}, state()) -> {reply, ok, state()} | {reply, {error, unknown_call}, state()}.
handle_call({add_event, Event}, _From, State) ->
    #{time := Time, action := Action, user_id := UserId} = Event,
    Entry = #entry{time = Time, action = Action, user_id = UserId},
    true = ets:insert(?TAB, Entry),
    {reply, ok, State};
handle_call({set_timer, Millis}, _From, #{timer := Timer} = State) ->
    {ok, cancel} = timer:cancel(Timer),
    NewTimer = set_timer(Millis),
    {reply, ok, maps:update(timer, NewTimer, State)};
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
terminate(Reason, _State) ->
    lager:error(Reason).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

create_table() ->
    ets:new(?TAB, [
        bag,
        public,
        named_table,
        {keypos, #entry.time}
    ]).

set_timer() ->
    set_timer(?POLL_MILLIS).

set_timer(Millis) ->
    {ok, Timer} = timer:send_interval(Millis, check),
    Timer.

check() ->
    {Date, {H, M, _}} = calendar:universal_time(),
    Key = {Date, {H, M}},
    case ets:lookup(?TAB, Key) of
        [] ->
            ok;
        Objects ->
            lists:foreach(fun(Entry) ->
                fire(Entry),
                ets:delete(?TAB, Key)
            end, Objects)
    end.

fire(#entry{user_id = UserId, action = Action}) ->
    lager:debug("Fire [~p]", [Action]),
    reminder_bot_telegram:send_message(UserId, list_to_binary(Action)).
