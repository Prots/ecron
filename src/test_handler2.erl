-module(test_handler2).
-behaviour(ecron_handler).
-author("prots.igor@gmail.com").

%% API
-export([handle_task/1]).

handle_task(_Args) ->
    io:format("Time: ~p~n", [calendar:local_time()]).