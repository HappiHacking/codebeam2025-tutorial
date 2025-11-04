-module(process_monitor).
-behaviour(gen_server).

%% % Compile and start the monitor
%% c(process_monitor).
%% {ok, _Pid} = process_monitor:start_link().
%%
%% % Create test processes to trigger alerts
%%
%% % High memory usage
%% spawn(fun() ->
%%    Data = lists:seq(1, 1000000),
%%    timer:sleep(10000)
%% end).
%%
%% % Message backlog
%% spawn(fun() ->
%%    Self = self(),
%%    [Self ! test_message || _ <- lists:seq(1, 2000)],
%%    timer:sleep(10000)
%% end).
%%
%% % Check results after a few scans
%% timer:sleep(6000).
%% process_monitor:get_stats().



%% API
-export([start_link/0, start_link/1, get_stats/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    alerts = [] :: [string()],
    last_scan = undefined :: undefined | integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    start_link([]).

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

get_stats() ->
    gen_server:call(?MODULE, get_stats).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(_Opts) ->
    timer:send_interval(5000, scan_processes),
    {ok, #state{}}.

handle_info(scan_processes, State) ->
    Alerts = scan_for_issues(),

    % Log new alerts
    NewAlerts = Alerts -- State#state.alerts,
    lists:foreach(
        fun(Alert) ->
            io:format("🚨 ALERT: ~s~n", [Alert])
        end,
        NewAlerts
    ),

    {noreply, State#state{
        alerts = Alerts,
        last_scan = erlang:system_time(second)
    }};

handle_info(_Info, State) ->
    {noreply, State}.

handle_call(get_stats, _From, State) ->
    Stats = #{
        total_processes => length(erlang:processes()),
        active_alerts => length(State#state.alerts),
        last_scan => State#state.last_scan,
        alerts => State#state.alerts
    },
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

scan_for_issues() ->
    Processes = erlang:processes(),
    lists:flatmap(fun check_process/1, Processes).

check_process(Pid) ->
    case erlang:process_info(Pid, [memory, message_queue_len, reductions, registered_name]) of
        undefined ->
            [];  % Process died during scan
        Info ->
            Memory = proplists:get_value(memory, Info),
            MsgQueueLen = proplists:get_value(message_queue_len, Info),
            Reductions = proplists:get_value(reductions, Info),

            Alerts0 = [],

            % Check memory usage (>50MB)
            Alerts1 = if
                Memory > 50 * 1024 * 1024 ->
                    MemMB = Memory div (1024 * 1024),
                    Msg = io_lib:format("High memory: ~s using ~pMB",
                                       [format_process(Pid, Info), MemMB]),
                    [lists:flatten(Msg) | Alerts0];
                true ->
                    Alerts0
            end,

            % Check message queue (>1000 messages)
            Alerts2 = if
                MsgQueueLen > 1000 ->
                    Msg = io_lib:format("Message backlog: ~s has ~p queued messages",
                                       [format_process(Pid, Info), MsgQueueLen]),
                    [lists:flatten(Msg) | Alerts1];
                true ->
                    Alerts1
            end,

            % Check reduction rate (heuristic for CPU-bound processes)
            Alerts3 = if
                Reductions > 1000000 ->
                    Msg = io_lib:format("High CPU: ~s consumed ~p reductions",
                                       [format_process(Pid, Info), Reductions]),
                    [lists:flatten(Msg) | Alerts2];
                true ->
                    Alerts2
            end,

            Alerts3
    end.

format_process(Pid, Info) ->
    case proplists:get_value(registered_name, Info) of
        undefined ->
            io_lib:format("~p", [Pid]);
        Name ->
            io_lib:format("~p (~p)", [Name, Pid])
    end.