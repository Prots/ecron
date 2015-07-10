-module(ecron_handlers_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).
-export([start_handler/2, stop_handler/1]).

%% API
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_handler(Name, Opts) ->
    {ok, HandlerPid} = supervisor:start_child(?MODULE, [[Name|Opts]]),
    io:format("Start handler: ~p, with options: ~p, PID: ~p~n", [Name, Opts, HandlerPid]),
    ok.

stop_handler(HandlerPid) ->
    io:format("Stop handler with ID: ~p~n", [HandlerPid]),
    ok = supervisor:terminate_child(ecron_handlers_sup, HandlerPid).

%% supervisor callbacks
init([]) ->
    Handler = {
        ecron_handler,
        {ecron_handler, start_link, []},
        transient, brutal_kill, worker, [ecron_handler]
    },
    {ok, {{simple_one_for_one, 250, 5}, [Handler]}}.