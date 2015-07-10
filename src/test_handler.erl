-module(test_handler).
-behaviour(ecron_handler).
-author("prots.igor@gmail.com").

%% API
-export([handle_task/1]).

handle_task(Args) ->
    [Arg1, Arg2] = Args,
    io:format(Arg1, Arg2).
