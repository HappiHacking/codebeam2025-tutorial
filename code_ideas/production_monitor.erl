-module(production_monitor).

%% API exports
-export([
    health_check/0,
    check_memory_health/0,
    check_process_health/0,
    check_scheduler_health/0,
    check_allocation_health/0,
    check_alerts/1
]).

%%%===================================================================
%%% API
%%%===================================================================

health_check() ->
    #{
        timestamp => calendar:universal_time(),
        memory => check_memory_health(),
        processes => check_process_health(),
        schedulers => check_scheduler_health(),
        allocation => check_allocation_health()
    }.

check_memory_health() ->
    Memory = recon_alloc:memory(),
    TotalMiB = proplists:get_value(total, Memory) / (1024 * 1024),
    UsedMiB = proplists:get_value(used, Memory) / (1024 * 1024),
    UsagePercent = (UsedMiB / TotalMiB) * 100,

    Status = if
        UsagePercent > 90 -> critical;
        UsagePercent > 80 -> warning;
        true -> ok
    end,

    #{
        total_mib => round(TotalMiB * 100) / 100,
        used_mib => round(UsedMiB * 100) / 100,
        usage_percent => round(UsagePercent * 100) / 100,
        status => Status
    }.

check_process_health() ->
    TotalProcesses = length(erlang:processes()),
    Limit = erlang:system_info(process_limit),
    UsagePercent = (TotalProcesses / Limit) * 100,

    % Find problematic processes
    MemoryHogs = recon:proc_count(memory, 5),
    QueueBackups = recon:proc_count(message_queue_len, 5),
    CpuIntensive = recon:proc_count(reductions, 5),

    Status = if
        UsagePercent > 90 -> critical;
        UsagePercent > 80 -> warning;
        TotalProcesses > 100000 -> warning;
        true -> ok
    end,

    #{
        total => TotalProcesses,
        limit => Limit,
        usage_percent => round(UsagePercent * 100) / 100,
        memory_hogs => format_proc_info(MemoryHogs),
        queue_backups => format_proc_info(QueueBackups),
        cpu_intensive => format_proc_info(CpuIntensive),
        status => Status
    }.

check_scheduler_health() ->
    Schedulers = erlang:system_info(schedulers),
    RunQueues = erlang:statistics(run_queue_lengths),
    TotalQueued = lists:sum(RunQueues),

    AvgQueueLength = if
        Schedulers > 0 -> TotalQueued / Schedulers;
        true -> 0
    end,

    Status = if
        TotalQueued > Schedulers * 100 -> critical;
        TotalQueued > Schedulers * 10 -> warning;
        true -> ok
    end,

    #{
        scheduler_count => Schedulers,
        run_queue_lengths => RunQueues,
        total_queued => TotalQueued,
        avg_queue_length => round(AvgQueueLength * 100) / 100,
        status => Status
    }.

check_allocation_health() ->
    Fragmentation = recon_alloc:fragmentation(current),

    % Calculate average fragmentation across allocators
    Frags = lists:filtermap(
        fun({_Allocator, AllocData}) ->
            Usage = maps:get(usage, AllocData, 0),
            Allocated = maps:get(allocated, AllocData, 0),
            if
                Allocated > 0 ->
                    FragPercent = (1.0 - Usage / Allocated) * 100,
                    {true, FragPercent};
                true ->
                    false
            end
        end,
        Fragmentation
    ),

    AvgFragmentation = case Frags of
        [] -> 0.0;
        _ -> lists:sum(Frags) / length(Frags)
    end,

    Status = if
        AvgFragmentation > 50 -> warning;
        AvgFragmentation > 30 -> caution;
        true -> ok
    end,

    #{
        fragmentation_percent => round(AvgFragmentation * 100) / 100,
        allocators => Fragmentation,
        status => Status
    }.

check_alerts(HealthData) ->
    Alerts0 = [],

    % Check memory status
    MemoryStatus = maps:get(status, maps:get(memory, HealthData)),
    Alerts1 = case lists:member(MemoryStatus, [warning, critical]) of
        true ->
            MemoryUsage = maps:get(usage_percent, maps:get(memory, HealthData)),
            Msg = io_lib:format("Memory usage at ~.2f%", [MemoryUsage]),
            [lists:flatten(Msg) | Alerts0];
        false ->
            Alerts0
    end,

    % Check process status
    ProcessStatus = maps:get(status, maps:get(processes, HealthData)),
    Alerts2 = case lists:member(ProcessStatus, [warning, critical]) of
        true ->
            ProcessUsage = maps:get(usage_percent, maps:get(processes, HealthData)),
            Msg = io_lib:format("Process count at ~.2f%", [ProcessUsage]),
            [lists:flatten(Msg) | Alerts1];
        false ->
            Alerts1
    end,

    % Check scheduler status
    SchedulerStatus = maps:get(status, maps:get(schedulers, HealthData)),
    Alerts3 = case lists:member(SchedulerStatus, [warning, critical]) of
        true ->
            TotalQueued = maps:get(total_queued, maps:get(schedulers, HealthData)),
            Msg = io_lib:format("Run queues backed up: ~p total", [TotalQueued]),
            [lists:flatten(Msg) | Alerts2];
        false ->
            Alerts2
    end,

    Alerts3.

%%%===================================================================
%%% Internal functions
%%%===================================================================

format_proc_info(ProcList) ->
    lists:map(
        fun({Pid, Value, Info}) ->
            Name = case proplists:get_value(registered_name, Info) of
                undefined -> Pid;
                RegName when is_atom(RegName) -> RegName;
                _ -> Pid
            end,
            CurrentFunction = proplists:get_value(current_function, Info),
            {Name, Value, CurrentFunction}
        end,
        ProcList
    ).
