-module(metrics_collector).
-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, get_current_metrics/0, get_metrics_history/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    history = [] :: [map()],
    max_history = 100 :: pos_integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    start_link([]).

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

get_current_metrics() ->
    gen_server:call(?MODULE, get_metrics).

get_metrics_history() ->
    gen_server:call(?MODULE, get_history).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    Interval = proplists:get_value(interval, Opts, 10000), % 10 seconds default
    timer:send_interval(Interval, collect_metrics),

    MaxHistory = proplists:get_value(max_history, Opts, 100),
    {ok, #state{max_history = MaxHistory}}.

handle_info(collect_metrics, State) ->
    Metrics = system_metrics:collect_all(),

    NewHistory = lists:sublist([Metrics | State#state.history], State#state.max_history),

    {noreply, State#state{history = NewHistory}};

handle_info(_Info, State) ->
    {noreply, State}.

handle_call(get_metrics, _From, State) ->
    Current = case State#state.history of
        [Latest | _] -> Latest;
        [] -> system_metrics:collect_all()
    end,
    {reply, Current, State};

handle_call(get_history, _From, State) ->
    {reply, State#state.history, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
