-module(ecron_manager).
-author("prots.igor@gmail.com").

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-include("ecron.hrl").

-record(state, {timer_ref, check_interval, task_list}).

%% API
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% gen_server callbacks
init(_Args) ->
    {ok, TaskList} = application:get_env(ecron, task_list),
    {ok, TRef} = timer:send_after(1, check),
    {ok, #state{timer_ref = TRef, check_interval = ?TICK, task_list = TaskList}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check, #state{timer_ref = TRef, check_interval = Interval, task_list = TaskList} = State) ->
    {ok, cancel} = timer:cancel(TRef),
    _ = check_tasks_for_start(calendar:local_time(), TaskList),
    {ok, NewTRef} = timer:send_after(Interval, check),
    {noreply, State#state{timer_ref = NewTRef}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal

check_tasks_for_start(DateTime, TaskList) ->
     [begin
          case ecron_core:is_time_to_go(DateTime, CronSpec) of
              true -> ecron_handlers_sup:start_handler(Module, Args);
              false -> ok
          end
      end || {CronSpec, Module, Args} <- TaskList].
