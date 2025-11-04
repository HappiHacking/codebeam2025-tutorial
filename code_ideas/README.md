# BEAM Tutorial Code Examples

This directory contains Erlang code modules referenced in the BEAM tutorial course.

## Module 5: Debugging & Observability

### Process Monitor

**File:** [process_monitor.erl](process_monitor.erl)

A GenServer that continuously scans processes and alerts on suspicious behavior:
- High memory usage (>50 MiB)
- Message queue backlogs (>1000 messages)
- High CPU consumption (>1M reductions)

**Usage:**

```erlang
% Compile and start
c(process_monitor).
{ok, _Pid} = process_monitor:start_link().

% Create test processes to trigger alerts
spawn(fun() ->
    Data = lists:seq(1, 1000000),
    timer:sleep(10000)
end).

spawn(fun() ->
    Self = self(),
    [Self ! test_message || _ <- lists:seq(1, 2000)],
    timer:sleep(10000)
end).

% Check stats
timer:sleep(6000).
process_monitor:get_stats().
```

### System Metrics Collection

**Files:**
- [system_metrics.erl](system_metrics.erl) - Comprehensive BEAM metrics collection
- [metrics_collector.erl](metrics_collector.erl) - GenServer for periodic metrics gathering

The `system_metrics` module collects:
- **Process metrics**: count, memory distribution, message queue lengths, reductions
- **Memory metrics**: total, processes, system, atom, binary, code, ETS
- **Scheduler metrics**: utilization per scheduler, run queue lengths
- **System metrics**: uptime, reductions, runtime, GC stats, context switches
- **I/O metrics**: input/output bytes, port count and limits

The `metrics_collector` GenServer runs periodic collection and maintains a rolling history.

**Usage:**

```erlang
% Compile the modules
c(system_metrics).
c(metrics_collector).

% Start collecting metrics (every 5 seconds)
{ok, _Pid} = metrics_collector:start_link([{interval, 5000}]).

% Wait for a few collections
timer:sleep(6000).

% View current metrics
Metrics = metrics_collector:get_current_metrics(),
io:format("Memory usage: ~p~n", [maps:get(memory, Metrics)]),
io:format("Process stats: ~p~n", [maps:get(processes, Metrics)]).

% Get historical data
History = metrics_collector:get_metrics_history(),
io:format("Collected ~p snapshots~n", [length(History)]).

% Collect one-time metrics
AllMetrics = system_metrics:collect_all(),
io:format("Scheduler metrics: ~p~n", [maps:get(schedulers, AllMetrics)]).
```

**Example output:**

```erlang
% Memory metrics
#{atom => 1577457,
  atom_used => 1553482,
  binary => 318848,
  code => 25896134,
  ets => 2834488,
  processes => 9461384,
  processes_used => 9459232,
  system => 52428800,
  total => 88119832}

% Process metrics
#{count => 127,
  memory_distribution =>
    #{count => 127,
      max => 1048576,
      median => 4320,
      min => 352,
      p95 => 87640,
      p99 => 524288,
      total => 9461384},
  queue_lengths =>
    #{count => 127, max => 0, median => 0, min => 0, p95 => 0, p99 => 0, total => 0},
  reductions =>
    #{avg => 15234.5, max => 1234567, total => 1934789}}
```

### Production Monitor (Recon-based)

**File:** [production_monitor.erl](production_monitor.erl)

Production-ready health monitoring using the [Recon](https://github.com/ferd/recon) library by Fred Hebert. Recon is the go-to library for production BEAM debugging and diagnostics.

**Prerequisites:** Requires the `recon` library to be installed.

**Installing Recon:**

For Erlang projects using rebar3, add to `rebar.config`:
```erlang
{deps, [
    {recon, "2.5.5"}
]}.
```

For Elixir projects, add to `mix.exs`:
```elixir
{:recon, "~> 2.5"}
```

**Recon Quick Reference:**

```erlang
% Top processes by various criteria
recon:proc_count(memory, 10).           % Top 10 by memory
recon:proc_count(reductions, 5).        % Top 5 by reductions
recon:proc_count(message_queue_len, 3). % Top 3 by queue length

% Memory allocation info
recon_alloc:memory().
recon_alloc:memory(allocated).
recon_alloc:fragmentation(current).

% System info
recon:info().
recon:node_stats().
```

**Features:**
- **Memory health**: Total/used memory with status thresholds (ok/warning/critical)
- **Process health**: Process count, top consumers by memory/queue/CPU
- **Scheduler health**: Run queue analysis and load distribution
- **Allocation health**: Memory fragmentation analysis across allocators
- **Smart alerting**: Automatic alert generation based on thresholds

**Usage:**

```erlang
% Compile (requires recon)
c(production_monitor).

% Run a comprehensive health check
Health = production_monitor:health_check(),
io:format("System health: ~p~n", [Health]).

% Check specific subsystems
MemoryHealth = production_monitor:check_memory_health(),
io:format("Memory: ~.2f MiB / ~.2f MiB (~.2f%) - Status: ~p~n",
    [maps:get(used_mib, MemoryHealth),
     maps:get(total_mib, MemoryHealth),
     maps:get(usage_percent, MemoryHealth),
     maps:get(status, MemoryHealth)]).

ProcessHealth = production_monitor:check_process_health(),
io:format("Processes: ~p / ~p (~.2f%) - Status: ~p~n",
    [maps:get(total, ProcessHealth),
     maps:get(limit, ProcessHealth),
     maps:get(usage_percent, ProcessHealth),
     maps:get(status, ProcessHealth)]).

% Get top resource consumers
MemoryHogs = maps:get(memory_hogs, ProcessHealth),
io:format("Top memory consumers: ~p~n", [MemoryHogs]).

% Check for alerts
Alerts = production_monitor:check_alerts(Health),
case Alerts of
    [] ->
        io:format("✅ All systems normal~n");
    _ ->
        io:format("🚨 Active alerts:~n"),
        [io:format("  - ~s~n", [Alert]) || Alert <- Alerts]
end.
```

**Example output:**

```erlang
% Healthy system
Memory: 245.67 MiB / 512.00 MiB (47.98%) - Status: ok
Processes: 342 / 262144 (0.13%) - Status: ok
✅ All systems normal

% System under stress
Memory: 487.23 MiB / 512.00 MiB (95.16%) - Status: critical
Processes: 89234 / 262144 (34.04%) - Status: ok
🚨 Active alerts:
  - Memory usage at 95.16%
```

## Building and Running

All modules can be compiled from the Erlang shell:

```erlang
% Compile all modules
[c(M) || M <- [process_monitor, system_metrics, metrics_collector, production_monitor]].
```

Or use the Erlang compiler from the command line:

```bash
erlc *.erl
```

Then start an Erlang shell and run the examples:

```bash
erl
```
