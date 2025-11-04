-module(system_metrics).

%% API exports
-export([
    collect_all/0,
    collect_process_metrics/0,
    collect_memory_metrics/0,
    collect_scheduler_metrics/0,
    collect_system_metrics/0,
    collect_io_metrics/0
]).

%%%===================================================================
%%% API
%%%===================================================================

collect_all() ->
    #{
        timestamp => erlang:system_time(second),
        processes => collect_process_metrics(),
        memory => collect_memory_metrics(),
        schedulers => collect_scheduler_metrics(),
        system => collect_system_metrics(),
        io => collect_io_metrics()
    }.

collect_process_metrics() ->
    Processes = erlang:processes(),

    #{
        count => length(Processes),
        memory_distribution => process_memory_distribution(Processes),
        queue_lengths => message_queue_distribution(Processes),
        reductions => process_reduction_stats(Processes)
    }.

collect_memory_metrics() ->
    MemoryInfo = erlang:memory(),

    #{
        total => proplists:get_value(total, MemoryInfo),
        processes => proplists:get_value(processes, MemoryInfo),
        processes_used => proplists:get_value(processes_used, MemoryInfo),
        system => proplists:get_value(system, MemoryInfo),
        atom => proplists:get_value(atom, MemoryInfo),
        atom_used => proplists:get_value(atom_used, MemoryInfo),
        binary => proplists:get_value(binary, MemoryInfo),
        code => proplists:get_value(code, MemoryInfo),
        ets => proplists:get_value(ets, MemoryInfo)
    }.

collect_scheduler_metrics() ->
    SchedulerCount = erlang:system_info(schedulers),
    SchedulerUsage = erlang:statistics(scheduler_wall_time),

    Utilizations = case SchedulerUsage of
        undefined ->
            % Enable scheduler wall time tracking
            erlang:system_flag(scheduler_wall_time, true),
            [];
        Usage ->
            calculate_scheduler_utilization(Usage)
    end,

    #{
        scheduler_count => SchedulerCount,
        utilization => Utilizations,
        run_queue_lengths => erlang:statistics(run_queue_lengths),
        total_run_queue_lengths => erlang:statistics(total_run_queue_lengths)
    }.

collect_system_metrics() ->
    {Uptime, _} = erlang:statistics(wall_clock),
    {TotalReds, _} = erlang:statistics(reductions),
    {Runtime, _} = erlang:statistics(runtime),
    {NumGCs, WordsReclaimed, _} = erlang:statistics(garbage_collection),
    {ContextSwitches, _} = erlang:statistics(context_switches),

    #{
        uptime => Uptime,
        reductions => TotalReds,
        runtime => Runtime,
        garbage_collection => #{num_gcs => NumGCs, words_reclaimed => WordsReclaimed},
        context_switches => ContextSwitches,
        io => erlang:statistics(io)
    }.

collect_io_metrics() ->
    {{input, Input}, {output, Output}} = erlang:statistics(io),

    #{
        input_bytes => Input,
        output_bytes => Output,
        port_count => erlang:system_info(port_count),
        port_limit => erlang:system_info(port_limit)
    }.

%%%===================================================================
%%% Internal functions
%%%===================================================================

process_memory_distribution(Processes) ->
    Memories = lists:map(
        fun(Pid) ->
            case erlang:process_info(Pid, memory) of
                {memory, Memory} -> Memory;
                undefined -> 0
            end
        end,
        Processes
    ),
    calculate_distribution(Memories).

message_queue_distribution(Processes) ->
    QueueLengths = lists:map(
        fun(Pid) ->
            case erlang:process_info(Pid, message_queue_len) of
                {message_queue_len, Len} -> Len;
                undefined -> 0
            end
        end,
        Processes
    ),
    calculate_distribution(QueueLengths).

process_reduction_stats(Processes) ->
    Reductions = lists:map(
        fun(Pid) ->
            case erlang:process_info(Pid, reductions) of
                {reductions, Reds} -> Reds;
                undefined -> 0
            end
        end,
        Processes
    ),

    Total = lists:sum(Reductions),
    Max = case Reductions of
        [] -> 0;
        _ -> lists:max(Reductions)
    end,
    Avg = case length(Reductions) of
        0 -> 0;
        Count -> Total / Count
    end,

    #{
        total => Total,
        max => Max,
        avg => Avg
    }.

calculate_distribution(Values) ->
    Sorted = lists:sort(Values),
    Count = length(Sorted),

    Min = case Sorted of
        [] -> 0;
        _ -> lists:min(Sorted)
    end,

    Max = case Sorted of
        [] -> 0;
        _ -> lists:max(Sorted)
    end,

    #{
        min => Min,
        max => Max,
        median => percentile(Sorted, 0.5),
        p95 => percentile(Sorted, 0.95),
        p99 => percentile(Sorted, 0.99),
        total => lists:sum(Sorted),
        count => Count
    }.

percentile([], _) -> 0;
percentile(SortedList, Percentile) ->
    Count = length(SortedList),
    Index = round(Percentile * Count) - 1,
    SafeIndex = max(0, min(Index, Count - 1)),
    lists:nth(SafeIndex + 1, SortedList).

calculate_scheduler_utilization(SchedulerUsage) ->
    lists:map(
        fun({SchedulerId, ActiveTime, WallTime}) ->
            Utilization = if
                WallTime > 0 ->
                    (ActiveTime / WallTime) * 100;
                true ->
                    0.0
            end,

            #{
                scheduler_id => SchedulerId,
                utilization_percent => round(Utilization * 100) / 100
            }
        end,
        SchedulerUsage
    ).
