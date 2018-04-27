-module(reminder_bot_cowboy_sup).
-export([
    start_link/0
]).

-behaviour(supervisor_bridge).
-export([
    init/1,
    terminate/2
]).

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor_bridge:start_link({local, ?SERVER}, ?MODULE, []).

-spec init([]) -> {ok, pid(), map()}.
init([]) ->
    Port = port(),
    Dispatch = dispatch(),
    {ok, Pid} = cowboy:start_clear(reminder_bot_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    {ok, Pid, #{}}.

-spec terminate(term(), term()) -> ok.
terminate(_Reason, _State) ->
    ok.

port() ->
    case os:getenv("PORT") of
        false -> 8080;
        Port -> Port
    end.

dispatch() ->
    cowboy_router:compile([
        {'_', [{"/", reminder_bot_handler, []}]}
    ]).
