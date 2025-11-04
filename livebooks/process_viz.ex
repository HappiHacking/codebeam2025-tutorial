defmodule ProcessViz do
  @moduledoc """
  Visualization utilities for Erlang processes in Livebook using Kino.Mermaid.
  """

  @doc """
  Renders a parent-worker process tree.

  ## Parameters
  - parent_name: Atom name of the registered parent process
  - get_workers_fn: Function that queries the parent for worker list

  ## Example
      ProcessViz.render_parent_workers(:parent, fn ->
        send(:parent, {:status, self()})
        receive do {:workers, w} -> w after 1000 -> [] end
      end)
  """
  def render_parent_workers(parent_name, get_workers_fn) do
    parent_pid = Process.whereis(parent_name)
    workers = get_workers_fn.()

    worker_nodes = Enum.map_join(workers, "\n  ", fn {name, pid} ->
      status = if Process.alive?(pid), do: "✓", else: "✗"
      "#{parent_name} --> #{name}[\"#{name}<br/>#{inspect(pid)}<br/>#{status}\"]"
    end)

    mermaid = """
    graph TD
      #{parent_name}["#{parent_name}<br/>#{inspect(parent_pid)}"]
      #{worker_nodes}
    """

    Kino.Mermaid.new(mermaid)
  end

  @doc """
  Renders a supervisor tree with children.

  ## Parameters
  - supervisor_name: Atom name of the registered supervisor process
  - get_children_fn: Function that queries the supervisor for children list
  - max_restarts: Maximum number of restarts allowed (optional)

  ## Example
      ProcessViz.render_supervisor(:supervisor, fn ->
        send(:supervisor, {:get_children, self()})
        receive do {:children, c} -> c after 1000 -> [] end
      end, 3)
  """
  def render_supervisor(supervisor_name, get_children_fn, max_restarts \\ nil) do
    supervisor_pid = Process.whereis(supervisor_name)
    children = get_children_fn.()

    child_nodes = Enum.map_join(children, "\n  ", fn {name, pid, restarts} ->
      status = if Process.alive?(pid), do: "✓", else: "✗"
      color = cond do
        !Process.alive?(pid) -> "style #{name} fill:#ff6b6b"
        restarts > 0 -> "style #{name} fill:#ffd93d"
        true -> ""
      end

      node = "#{supervisor_name} --> #{name}[\"#{name}<br/>#{inspect(pid)}<br/>restarts: #{restarts} #{status}\"]"
      if color != "", do: "#{node}\n  #{color}", else: node
    end)

    max_restart_info = if max_restarts, do: "<br/>max_restarts: #{max_restarts}", else: ""

    mermaid = """
    graph TD
      #{supervisor_name}["Supervisor: #{supervisor_name}<br/>#{inspect(supervisor_pid)}#{max_restart_info}"]
      #{child_nodes}
    """

    Kino.Mermaid.new(mermaid)
  end

  @doc """
  Renders a ring topology of processes.

  ## Parameters
  - node_count: Number of nodes in the ring
  - first_pid: PID of the first node (optional)

  ## Example
      ProcessViz.render_ring(5, Process.whereis(:ring_first))
  """
  def render_ring(node_count, first_pid \\ nil) do
    edges = for i <- 1..node_count do
      next = if i == node_count, do: 1, else: i + 1
      "node#{i}[\"Node #{i}\"] --> node#{next}"
    end

    first_node_info = if first_pid do
      "\n  style node1 fill:#6bcf7f\n  note1[\"First: #{inspect(first_pid)}\"]:::note\n  note1 -.-> node1\n  classDef note fill:#e3f2fd,stroke:#1976d2"
    else
      ""
    end

    mermaid = """
    graph LR
      #{Enum.join(edges, "\n  ")}#{first_node_info}
    """

    Kino.Mermaid.new(mermaid, caption: "Ring Topology - #{node_count} Nodes")
  end

  @doc """
  Renders a process registry with registered services.

  ## Parameters
  - registry_name: Atom name of the registered registry process
  - get_services_fn: Function that queries the registry for service list

  ## Example
      ProcessViz.render_registry(:registry, fn ->
        send(:registry, {:list, self()})
        receive do {:process_list, p} -> p after 1000 -> [] end
      end)
  """
  def render_registry(registry_name, get_services_fn) do
    registry_pid = Process.whereis(registry_name)
    services = get_services_fn.()

    service_nodes = Enum.map_join(services, "\n  ", fn {name, pid} ->
      status = if Process.alive?(pid), do: "✓", else: "✗"
      color = if Process.alive?(pid), do: "", else: "\n  style #{name} fill:#ff6b6b"
      "#{registry_name} --> #{name}[\"#{name}<br/>#{inspect(pid)}<br/>#{status}\"]#{color}"
    end)

    mermaid = """
    graph TD
      #{registry_name}["Registry<br/>#{inspect(registry_pid)}"]
      #{service_nodes}
    """

    Kino.Mermaid.new(mermaid)
  end

  @doc """
  Renders a parallel map execution showing workers processing elements.

  ## Parameters
  - element_count: Number of elements being processed
  - caption: Optional caption for the diagram

  ## Example
      ProcessViz.render_parallel_map(6, "Parallel Map: Squaring Numbers")
  """
  def render_parallel_map(element_count, caption \\ "Parallel Map Execution") do
    workers = for i <- 1..element_count do
      "parent[\"Parent Process\"] --> worker#{i}[\"Worker #{i}<br/>Element #{i}\"]"
    end

    results = for i <- 1..element_count do
      "worker#{i} -.->|result| parent"
    end

    mermaid = """
    graph TD
      #{Enum.join(workers, "\n  ")}
      #{Enum.join(results, "\n  ")}
      style parent fill:#6bcf7f
    """

    Kino.Mermaid.new(mermaid, caption: caption)
  end

  @doc """
  Renders a generic process tree from a nested structure.

  ## Parameters
  - root_name: Name of the root process
  - root_pid: PID of the root process
  - children: List of child processes as {name, pid} or {name, pid, extra_info}

  ## Example
      ProcessViz.render_process_tree("parent", parent_pid, [
        {"child1", pid1},
        {"child2", pid2}
      ])
  """
  def render_process_tree(root_name, root_pid, children) do
    child_nodes = Enum.map_join(children, "\n  ", fn
      {name, pid} ->
        status = if Process.alive?(pid), do: "✓", else: "✗"
        "root --> #{name}[\"#{name}<br/>#{inspect(pid)}<br/>#{status}\"]"

      {name, pid, extra} ->
        status = if Process.alive?(pid), do: "✓", else: "✗"
        "root --> #{name}[\"#{name}<br/>#{inspect(pid)}<br/>#{extra}<br/>#{status}\"]"
    end)

    mermaid = """
    graph TD
      root["#{root_name}<br/>#{inspect(root_pid)}"]
      #{child_nodes}
    """

    Kino.Mermaid.new(mermaid)
  end

  @doc """
  Renders detailed process state information using a Mermaid diagram.

  Shows the Process Control Block (PCB), heap/stack memory layout, and mailbox.

  ## Parameters
  - pid: Process PID or registered name atom
  - opts: Options
    - :style - Diagram style, either :graph (default) or :block
    - :show_pcb - Show Process Control Block (default: true)
    - :show_memory - Show memory layout (default: true)
    - :show_mailbox - Show mailbox (default: true)

  ## Example
      ProcessViz.render_process_state(:counter)
      ProcessViz.render_process_state(:counter, style: :block)
      ProcessViz.render_process_state(:counter, show_pcb: true, show_memory: false, show_mailbox: false)
  """
  def render_process_state(pid, opts \\ [])

  def render_process_state(pid, opts) when is_atom(pid) do
    render_process_state(Process.whereis(pid), opts)
  end

  def render_process_state(pid, opts) when is_pid(pid) do
    info = Process.info(pid)
    style = Keyword.get(opts, :style, :graph)
    show_pcb = Keyword.get(opts, :show_pcb, true)
    show_memory = Keyword.get(opts, :show_memory, true)
    show_mailbox = Keyword.get(opts, :show_mailbox, true)

    if info do
      # Extract key metrics
      status = Keyword.get(info, :status, :unknown)
      reductions = Keyword.get(info, :reductions, 0)
      message_queue_len = Keyword.get(info, :message_queue_len, 0)
      heap_size = Keyword.get(info, :heap_size, 0)
      stack_size = Keyword.get(info, :stack_size, 0)
      total_heap_size = Keyword.get(info, :total_heap_size, 0)
      links = Keyword.get(info, :links, []) |> length()
      monitors = Keyword.get(info, :monitors, []) |> length()

      # Format sizes in words
      heap_kb = Float.round(heap_size * 8 / 1024, 2)
      stack_kb = Float.round(stack_size * 8 / 1024, 2)
      total_kb = Float.round(total_heap_size * 8 / 1024, 2)

      mermaid = case style do
        :block ->
          render_process_state_block(pid, status, reductions, message_queue_len,
                                     heap_size, stack_size, total_kb, heap_kb, stack_kb,
                                     links, monitors, show_pcb, show_memory, show_mailbox)
        _ ->
          render_process_state_graph(pid, status, reductions, message_queue_len,
                                     heap_size, stack_size, total_kb, heap_kb, stack_kb,
                                     links, monitors, show_pcb, show_memory, show_mailbox)
      end

      Kino.Mermaid.new(mermaid, caption: "Process State: #{inspect(pid)} [#{style}]")
    else
      Kino.Mermaid.new("graph TD\n  error[\"Process not alive\"]",
        caption: "Process #{inspect(pid)} - Not Found")
    end
  end

  # Original graph-based visualization
  defp render_process_state_graph(pid, status, reductions, message_queue_len,
                                   heap_size, _stack_size, total_kb, heap_kb, stack_kb,
                                   links, monitors, show_pcb, show_memory, show_mailbox) do
    pcb_section = if show_pcb do
      """
        subgraph PCB["Process Control Block"]
          pid["PID: #{inspect(pid)}"]
          state["Status: #{status}"]
          reds["Reductions: #{format_number(reductions)}"]
          lnks["Links: #{links} | Monitors: #{monitors}"]
        end
      """
    else
      ""
    end

    memory_section = if show_memory do
      """
        subgraph Memory["Memory Layout"]
          stack["Stack #{heap_size} words / #{stack_kb} KB<br/>grows downward ▼<br/>- Return addresses<br/>- Local variables"]
          free["Free Space"]
          heap["Heap #{heap_size} words / #{heap_kb} KB<br/>grows upward ▲<br/>- Terms, lists<br/>- Closures<br/>Total: #{total_kb} KB"]
        end
      """
    else
      ""
    end

    mailbox_section = if show_mailbox do
      """
        subgraph Mailbox["Mailbox"]
          mq["Messages: #{message_queue_len}"]
        end
      """
    else
      ""
    end

    # Build connections based on what's visible
    connections = []
    connections = if show_pcb && show_memory, do: connections ++ ["PCB --> Memory"], else: connections
    connections = if show_pcb && show_mailbox, do: connections ++ ["PCB --> Mailbox"], else: connections
    connections_str = if length(connections) > 0, do: "\n\n      #{Enum.join(connections, "\n      ")}", else: ""

    # Only include styles for visible sections
    styles_list = []
    styles_list = if show_pcb, do: styles_list ++ ["style PCB fill:#e3f2fd,stroke:#1976d2"], else: styles_list
    styles_list = if show_memory, do: styles_list ++ [
      "style Memory fill:#f3e5f5,stroke:#7b1fa2",
      "style stack fill:#ffcdd2",
      "style heap fill:#c8e6c9",
      "style free fill:#f5f5f5"
    ], else: styles_list
    styles_list = if show_mailbox, do: styles_list ++ ["style Mailbox fill:#fff3e0,stroke:#f57c00"], else: styles_list

    styles = if length(styles_list) > 0 do
      "\n\n      #{Enum.join(styles_list, "\n      ")}"
    else
      ""
    end

    """
    graph TB
    #{pcb_section}
    #{memory_section}
    #{mailbox_section}#{connections_str}#{styles}
    """
  end

  # Compact block diagram visualization
  defp render_process_state_block(pid, status, reductions, message_queue_len,
                                   _heap_size, _stack_size, total_kb, heap_kb, stack_kb,
                                   links, monitors, show_pcb, show_memory, show_mailbox) do
    # Calculate total columns needed
    _sections_count = Enum.count([show_pcb, show_memory, show_mailbox], & &1)

    # Calculate column widths
    {columns, pcb_cols, mem_cols, mb_cols} = case {show_pcb, show_memory, show_mailbox} do
      {true, true, true} -> {3, 3, 1, 1}
      {true, true, false} -> {2, 2, 1, 0}
      {true, false, true} -> {2, 2, 0, 1}
      {false, true, true} -> {2, 0, 1, 1}
      {true, false, false} -> {1, 1, 0, 0}
      {false, true, false} -> {1, 0, 1, 0}
      {false, false, true} -> {1, 0, 0, 1}
      {false, false, false} -> {1, 1, 0, 0}  # default to PCB if nothing selected
    end

    pcb_block = if show_pcb do
      """
        block:PCB:#{pcb_cols}
          columns 2
          p["PID: #{inspect(pid)}"]:2
          s["Status: #{status}"]
          r["Reds: #{format_number(reductions)}"]
          l["Links: #{links}"]
          m["Monitors: #{monitors}"]
        end
      """
    else
      ""
    end

    mem_block = if show_memory do
      spacer = if show_pcb, do: "  space\n", else: ""
      """
      #{spacer}  block:MEM:#{mem_cols}
          columns 1
          stk["Stack<br/>#{stack_kb} KB<br/>▼"]
          fr["Free"]
          hp["Heap<br/>#{heap_kb} KB<br/>▲"]
          tot["Total: #{total_kb} KB"]
        end
      """
    else
      ""
    end

    mb_block = if show_mailbox do
      spacer = if show_pcb || show_memory, do: "  space\n", else: ""
      """
      #{spacer}  block:MB:#{mb_cols}
          columns 1
          mq["Mailbox<br/>#{message_queue_len}<br/>messages"]
        end
      """
    else
      ""
    end

    # Only define classes and apply them if sections are visible
    class_defs = """
      classDef pcbClass fill:#e3f2fd,stroke:#1976d2,stroke-width:2px
      classDef memClass fill:#f3e5f5,stroke:#7b1fa2,stroke-width:2px
      classDef mbClass fill:#fff3e0,stroke:#f57c00,stroke-width:2px
      classDef stackClass fill:#ffcdd2,stroke:#c62828
      classDef heapClass fill:#c8e6c9,stroke:#2e7d32
      classDef freeClass fill:#f5f5f5,stroke:#9e9e9e
    """

    class_applications = []
    class_applications = if show_pcb, do: class_applications ++ ["class PCB pcbClass"], else: class_applications
    class_applications = if show_memory, do: class_applications ++ [
      "class MEM memClass",
      "class stk stackClass",
      "class hp heapClass",
      "class fr freeClass"
    ], else: class_applications
    class_applications = if show_mailbox, do: class_applications ++ ["class MB mbClass"], else: class_applications

    class_apps_str = if length(class_applications) > 0 do
      "\n\n      #{Enum.join(class_applications, "\n      ")}"
    else
      ""
    end

    """
    block-beta
      columns #{columns}
    #{pcb_block}#{mem_block}#{mb_block}
    #{class_defs}#{class_apps_str}
    """
  end

  @doc """
  Renders a detailed view of a process mailbox with message inspection.

  ## Parameters
  - pid: Process PID or registered name atom
  - opts: Options
    - :limit - Maximum number of messages to show (default: 10)

  ## Example
      ProcessViz.render_mailbox(:counter, limit: 5)
  """
  def render_mailbox(pid, opts \\ []) when is_atom(pid) or is_pid(pid) do
    actual_pid = if is_atom(pid), do: Process.whereis(pid), else: pid
    limit = Keyword.get(opts, :limit, 10)

    if actual_pid && Process.alive?(actual_pid) do
      info = Process.info(actual_pid, [:message_queue_len, :messages])
      queue_len = Keyword.get(info, :message_queue_len, 0)
      messages = Keyword.get(info, :messages, []) |> Enum.take(limit)

      message_nodes = messages
      |> Enum.with_index(1)
      |> Enum.map_join("\n  ", fn {msg, idx} ->
        msg_str = inspect(msg, limit: 50) |> String.replace("\"", "'")
        "mq --> msg#{idx}[\"#{idx}. #{msg_str}\"]"
      end)

      truncated = if queue_len > limit do
        "\n  mq --> more[\"... #{queue_len - limit} more messages\"]"
      else
        ""
      end

      mermaid = """
      graph TD
        mq["Mailbox<br/>#{inspect(actual_pid)}<br/>Total: #{queue_len} messages"]
        #{message_nodes}#{truncated}

        style mq fill:#fff3e0,stroke:#f57c00
      """

      Kino.Mermaid.new(mermaid, caption: "Mailbox - #{queue_len} message(s)")
    else
      Kino.Mermaid.new("graph TD\n  error[\"Process not alive\"]")
    end
  end

  @doc """
  Renders a comparison of multiple process states side-by-side.

  ## Parameters
  - processes: List of PIDs or registered names

  ## Example
      ProcessViz.compare_process_states([:counter, :supervisor, :registry])
  """
  def compare_process_states(processes) do
    data = Enum.map(processes, fn p ->
      pid = if is_atom(p), do: Process.whereis(p), else: p
      name = if is_atom(p), do: Atom.to_string(p), else: inspect(pid)

      if pid && Process.alive?(pid) do
        info = Process.info(pid)
        %{
          "Process" => name,
          "Status" => Keyword.get(info, :status, :unknown),
          "Reductions" => format_number(Keyword.get(info, :reductions, 0)),
          "Messages" => Keyword.get(info, :message_queue_len, 0),
          "Heap (KB)" => Float.round(Keyword.get(info, :heap_size, 0) * 8 / 1024, 2),
          "Stack (KB)" => Float.round(Keyword.get(info, :stack_size, 0) * 8 / 1024, 2),
          "Links" => Keyword.get(info, :links, []) |> length()
        }
      else
        %{
          "Process" => name,
          "Status" => :dead,
          "Reductions" => 0,
          "Messages" => 0,
          "Heap (KB)" => 0,
          "Stack (KB)" => 0,
          "Links" => 0
        }
      end
    end)

    Kino.DataTable.new(data)
  end

  @doc """
  Captures a snapshot of process memory state for comparison.

  ## Parameters
  - pid: Process PID or registered name atom

  ## Returns
  A map containing memory metrics that can be compared later

  ## Example
      snapshot1 = ProcessViz.capture_memory_snapshot(:counter)
      # ... do some work ...
      snapshot2 = ProcessViz.capture_memory_snapshot(:counter)
      ProcessViz.compare_memory_snapshots(snapshot1, snapshot2)
  """
  def capture_memory_snapshot(pid) when is_atom(pid) do
    capture_memory_snapshot(Process.whereis(pid))
  end

  def capture_memory_snapshot(pid) when is_pid(pid) do
    if Process.alive?(pid) do
      info = Process.info(pid, [
        :heap_size,
        :stack_size,
        :total_heap_size,
        :memory,
        :reductions,
        :message_queue_len,
        :garbage_collection
      ])

      gc_info = Keyword.get(info, :garbage_collection, [])

      heap_size = Keyword.get(info, :heap_size, 0)
      stack_size = Keyword.get(info, :stack_size, 0)
      total_heap_size = Keyword.get(info, :total_heap_size, 0)
      heap_block_size = Keyword.get(gc_info, :heap_block_size, 0)
      old_heap_block_size = Keyword.get(gc_info, :old_heap_block_size, 0)

      # Calculate free memory (allocated but not used)
      # heap_block_size is the allocated young heap, heap_size includes both young and old
      # Free space is approximately: allocated blocks - actual heap usage
      young_heap_free = max(heap_block_size - heap_size, 0)
      old_heap_free = max(old_heap_block_size - (total_heap_size - heap_size), 0)
      total_free = young_heap_free + old_heap_free

      %{
        pid: pid,
        timestamp: System.monotonic_time(:millisecond),
        heap_size: heap_size,
        stack_size: stack_size,
        total_heap_size: total_heap_size,
        heap_block_size: heap_block_size,
        old_heap_block_size: old_heap_block_size,
        free_heap: total_free,
        young_heap_free: young_heap_free,
        old_heap_free: old_heap_free,
        memory_bytes: Keyword.get(info, :memory, 0),
        reductions: Keyword.get(info, :reductions, 0),
        message_queue_len: Keyword.get(info, :message_queue_len, 0),
        minor_gcs: Keyword.get(gc_info, :minor_gcs, 0),
        fullsweep_after: Keyword.get(gc_info, :fullsweep_after, 0)
      }
    else
      nil
    end
  end

  @doc """
  Compares two memory snapshots and visualizes the differences.

  ## Parameters
  - snapshot1: First snapshot (earlier)
  - snapshot2: Second snapshot (later)

  ## Example
      snap1 = ProcessViz.capture_memory_snapshot(:counter)
      # ... do work ...
      snap2 = ProcessViz.capture_memory_snapshot(:counter)
      ProcessViz.compare_memory_snapshots(snap1, snap2)
  """
  def compare_memory_snapshots(snapshot1, snapshot2) do
    heap_diff = snapshot2.heap_size - snapshot1.heap_size
    stack_diff = snapshot2.stack_size - snapshot1.stack_size
    total_diff = snapshot2.total_heap_size - snapshot1.total_heap_size
    free_diff = snapshot2.free_heap - snapshot1.free_heap
    _mem_diff = snapshot2.memory_bytes - snapshot1.memory_bytes
    _red_diff = snapshot2.reductions - snapshot1.reductions
    msg_diff = snapshot2.message_queue_len - snapshot1.message_queue_len
    gc_diff = snapshot2.minor_gcs - snapshot1.minor_gcs

    time_diff = snapshot2.timestamp - snapshot1.timestamp

    # Create arrow indicators
    heap_arrow = if heap_diff > 0, do: "↑", else: (if heap_diff < 0, do: "↓", else: "→")
    stack_arrow = if stack_diff > 0, do: "↑", else: (if stack_diff < 0, do: "↓", else: "→")
    total_arrow = if total_diff > 0, do: "↑", else: (if total_diff < 0, do: "↓", else: "→")
    free_arrow = if free_diff > 0, do: "↑", else: (if free_diff < 0, do: "↓", else: "→")

    # Calculate heap utilization percentage
    util1 = if snapshot1.heap_block_size > 0 do
      Float.round((snapshot1.heap_size / snapshot1.heap_block_size) * 100, 1)
    else
      0.0
    end

    util2 = if snapshot2.heap_block_size > 0 do
      Float.round((snapshot2.heap_size / snapshot2.heap_block_size) * 100, 1)
    else
      0.0
    end

    # GC pressure indicator (low free memory suggests GC needed)
    gc_pressure1 = if snapshot1.free_heap < 100, do: "⚠️", else: ""
    gc_pressure2 = if snapshot2.free_heap < 100, do: "⚠️", else: ""

    mermaid = """
    block-beta
      columns 3
      block:BEFORE:1
        columns 1
        b1["BEFORE<br/>t=0ms"]
        b2["Heap Used: #{snapshot1.heap_size} words"]
        b3["Heap Free: #{snapshot1.free_heap} words #{gc_pressure1}"]
        b4["Stack: #{snapshot1.stack_size} words"]
        b5["Total: #{snapshot1.total_heap_size} words"]
        b6["Utilization: #{util1}%"]
        b7["Messages: #{snapshot1.message_queue_len}"]
        b8["GCs: #{snapshot1.minor_gcs}"]
      end
      block:DIFF:1
        columns 1
        d1["DELTA<br/>Δt=#{time_diff}ms"]
        d2["#{heap_arrow} #{format_diff(heap_diff)} words"]
        d3["#{free_arrow} #{format_diff(free_diff)} words"]
        d4["#{stack_arrow} #{format_diff(stack_diff)} words"]
        d5["#{total_arrow} #{format_diff(total_diff)} words"]
        d6["#{format_diff(util2 - util1)}%"]
        d7["#{format_diff(msg_diff)}"]
        d8["#{format_diff(gc_diff)} GCs"]
      end
      block:AFTER:1
        columns 1
        a1["AFTER<br/>t=#{time_diff}ms"]
        a2["Heap Used: #{snapshot2.heap_size} words"]
        a3["Heap Free: #{snapshot2.free_heap} words #{gc_pressure2}"]
        a4["Stack: #{snapshot2.stack_size} words"]
        a5["Total: #{snapshot2.total_heap_size} words"]
        a6["Utilization: #{util2}%"]
        a7["Messages: #{snapshot2.message_queue_len}"]
        a8["GCs: #{snapshot2.minor_gcs}"]
      end

      classDef beforeClass fill:#e8f5e9,stroke:#2e7d32
      classDef diffClass fill:#fff3e0,stroke:#f57c00
      classDef afterClass fill:#e3f2fd,stroke:#1976d2

      class BEFORE beforeClass
      class DIFF diffClass
      class AFTER afterClass
    """

    Kino.Mermaid.new(mermaid, caption: "Memory Change: #{inspect(snapshot1.pid)} (#{time_diff}ms elapsed)")
  end

  @doc """
  Tracks memory changes over time with multiple snapshots.

  ## Parameters
  - snapshots: List of memory snapshots (chronological order)

  ## Example
      snapshots = [snap1, snap2, snap3, snap4]
      ProcessViz.render_memory_timeline(snapshots)
  """
  def render_memory_timeline(snapshots) when is_list(snapshots) and length(snapshots) > 1 do
    first_timestamp = hd(snapshots).timestamp

    data = snapshots
    |> Enum.with_index()
    |> Enum.map(fn {snap, idx} ->
      utilization = if snap.heap_block_size > 0 do
        Float.round((snap.heap_size / snap.heap_block_size) * 100, 1)
      else
        0.0
      end

      gc_pressure = if snap.free_heap < 100, do: "⚠️", else: ""

      %{
        "Step" => idx,
        "Time (ms)" => snap.timestamp - first_timestamp,
        "Heap Used" => snap.heap_size,
        "Heap Free" => "#{snap.free_heap} #{gc_pressure}",
        "Stack" => snap.stack_size,
        "Total" => snap.total_heap_size,
        "Util %" => utilization,
        "Memory (KB)" => Float.round(snap.memory_bytes / 1024, 2),
        "Messages" => snap.message_queue_len,
        "GCs" => snap.minor_gcs,
        "Reductions" => format_number(snap.reductions)
      }
    end)

    Kino.DataTable.new(data)
  end

  @doc """
  Renders a detailed memory layout showing heap components.

  This uses `:erlang.process_info/2` with `:binary` and other options
  to show what's actually allocated on the heap.

  ## Parameters
  - pid: Process PID or registered name atom

  ## Example
      ProcessViz.render_heap_details(:counter)
  """
  def render_heap_details(pid) when is_atom(pid) do
    render_heap_details(Process.whereis(pid))
  end

  def render_heap_details(pid) when is_pid(pid) do
    if Process.alive?(pid) do
      info = Process.info(pid, [:binary, :heap_size, :total_heap_size, :garbage_collection])

      binaries = Keyword.get(info, :binary, [])
      heap_size = Keyword.get(info, :heap_size, 0)
      total_heap_size = Keyword.get(info, :total_heap_size, 0)
      gc_info = Keyword.get(info, :garbage_collection, [])

      # Calculate binary memory
      binary_memory = Enum.reduce(binaries, 0, fn {_id, size, _refs}, acc -> acc + size end)
      binary_count = length(binaries)

      # Get heap block info
      heap_block_size = Keyword.get(gc_info, :heap_block_size, 0)
      old_heap_block_size = Keyword.get(gc_info, :old_heap_block_size, 0)

      # Estimate components (approximate)
      young_heap = heap_size
      old_heap = total_heap_size - heap_size
      young_heap_free = max(heap_block_size - young_heap, 0)
      old_heap_free = max(old_heap_block_size - old_heap, 0)

      # Calculate utilization
      young_util = if heap_block_size > 0 do
        Float.round((young_heap / heap_block_size) * 100, 1)
      else
        0.0
      end

      old_util = if old_heap_block_size > 0 do
        Float.round((old_heap / old_heap_block_size) * 100, 1)
      else
        0.0
      end

      data = [
        %{
          "Component" => "Young Heap (Used)",
          "Size (words)" => young_heap,
          "Size (KB)" => Float.round(young_heap * 8 / 1024, 2),
          "Utilization" => "#{young_util}%"
        },
        %{
          "Component" => "Young Heap (Free)",
          "Size (words)" => young_heap_free,
          "Size (KB)" => Float.round(young_heap_free * 8 / 1024, 2),
          "Utilization" => "free"
        },
        %{
          "Component" => "Old Heap (Used)",
          "Size (words)" => old_heap,
          "Size (KB)" => Float.round(old_heap * 8 / 1024, 2),
          "Utilization" => "#{old_util}%"
        },
        %{
          "Component" => "Old Heap (Free)",
          "Size (words)" => old_heap_free,
          "Size (KB)" => Float.round(old_heap_free * 8 / 1024, 2),
          "Utilization" => "free"
        },
        %{
          "Component" => "Binaries (#{binary_count})",
          "Size (words)" => div(binary_memory, 8),
          "Size (KB)" => Float.round(binary_memory / 1024, 2),
          "Utilization" => "off-heap"
        },
        %{
          "Component" => "Total Allocated",
          "Size (words)" => heap_block_size + old_heap_block_size,
          "Size (KB)" => Float.round((heap_block_size + old_heap_block_size) * 8 / 1024, 2),
          "Utilization" => "blocks"
        }
      ]

      Kino.DataTable.new(data)
    else
      Kino.DataTable.new([%{"Error" => "Process not alive"}])
    end
  end

  # Helper to format differences with + or - sign
  defp format_diff(num) when num > 0, do: "+#{num}"
  defp format_diff(num) when num < 0, do: "#{num}"
  defp format_diff(_num), do: "0"

  # Helper to format large numbers with underscores
  defp format_number(num) when num >= 1_000_000 do
    "#{Float.round(num / 1_000_000, 2)}M"
  end

  defp format_number(num) when num >= 1_000 do
    "#{Float.round(num / 1_000, 2)}K"
  end

  defp format_number(num), do: to_string(num)
end
