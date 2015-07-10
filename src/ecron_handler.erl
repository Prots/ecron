-module(ecron_handler).
-author("prots.igor@gmail.com").

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% API
-export([start_link/1]).

-callback handle_task(Args::list(term())) -> 'ok'|tuple('error', Reason :: string()).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-record(state, {timer_ref, args}).

%% API
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%% gen_server
init(Args) ->
    {ok, TRef} = timer:send_after(1, go),
    {ok, #state{timer_ref = TRef, args = Args}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Reques, State) ->
    {noreply, State}.

handle_info(go, #state{timer_ref = TRef, args = [Module|Args]} = State) ->
    {ok, cancel} = timer:cancel(TRef),
    erlang:apply(Module, handle_task, [Args]),
    ok = ecron_handlers_sup:stop_handler(self()),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal



