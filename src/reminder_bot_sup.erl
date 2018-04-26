-module(reminder_bot_sup).

-export([start_link/0]).

-behaviour(supervisor).
-export([init/1]).

-define(SERVER, ?MODULE).
-define(FLAGS, #{
    strategy  => one_for_all,
    intensity => 5,
    period    => 10
}).
-define(CHILD(I, Type), #{
    id       => I,
    start    => {I, start_link, []},
    restart  => permanent,
    shutdown => 5000,
    type     => Type,
    modules  => [I]
}).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Children = [
      ?CHILD(reminder_bot_scheduler, worker)
    ],
    {ok, {?FLAGS, Children} }.
