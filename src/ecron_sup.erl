-module(ecron_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% API functions

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Supervisor callbacks

init([]) ->
    HandlersSup = {
        ecron_handlers_sup,
        {ecron_handlers_sup, start_link, []},
        transient, 5000, supervisor, [ecron_handlers_sup]
    },
    Mgr = {
        ecron_manager,
        {ecron_manager, start_link, []},
        permanent, 100, worker, [ecron_manager]
    },
    {ok, { {one_for_one, 5, 10}, [HandlersSup, Mgr]} }.

