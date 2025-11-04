# Chat Server: Binary Leak Demonstration

This OTP application demonstrates a common binary leak pattern in BEAM systems.

## The Problem

Chat rooms that relay messages (including images) can accumulate binary references without realizing it. Even though the room doesn't explicitly store the binaries, it holds references through:

1. **Received messages in mailbox** - Until GC runs
2. **Broadcast copies** - Each send creates a reference
3. **Minimal work = Rare GC** - Room does little computation, so GC doesn't trigger frequently

## Architecture

```
chat_server_app
    |
    +-- chat_server_sup (main supervisor)
            |
            +-- chat_room_sup (simple_one_for_one)
            |       |
            |       +-- chat_room (gen_server) [THE LEAK IS HERE]
            |
            +-- chat_client_sup (simple_one_for_one)
                    |
                    +-- chat_client (gen_server)
```

### Components

**chat_room.erl** - The room process that:
- Manages member list with monitoring
- Broadcasts messages to all members
- **PROBLEM**: Holds references to image binaries even though it doesn't "store" them
- Does minimal work, so GC rarely runs
- Accumulates ProcBin references over time

**chat_client.erl** - Client processes that:
- Send messages and images to room
- Receive broadcasts from room
- Do more work (formatting, displaying), trigger GC more often
- Handle cleanup properly

## Running the Demo

### Build and Start

```bash
cd chat_server
erlc -o ebin src/*.erl
erl -pa ebin -s demo run
```

### Interactive Demo

```erlang
% Start the application
application:start(chat_server).

% Create a room
{ok, _} = chat_server_sup:start_room(<<"general">>).

% Create clients
{ok, Alice} = chat_server_sup:start_client(<<"Alice">>, <<"general">>).
{ok, Bob} = chat_server_sup:start_client(<<"Bob">>, <<"general">>).

% Send text messages (normal)
chat_client:send_message(Alice, <<"Hello!">>).

% Check room stats
chat_room:get_stats(<<"general">>).

% Send an image (creates leak!)
Image = crypto:strong_rand_bytes(1024 * 1024).  % 1MB
chat_client:send_image(Alice, Image).

% Check stats again - memory grew!
chat_room:get_stats(<<"general">>).

% Send many images to see accumulation
[chat_client:send_image(Bob, crypto:strong_rand_bytes(1024*1024))
 || _ <- lists:seq(1, 10)].

% Watch memory grow
chat_room:get_stats(<<"general">>).
```

### Automated Demos

```erlang
% Basic demonstration
demo:run().

% More dramatic leak
demo:demonstrate_leak().

% Stress test with N images
demo:stress_test(50).
```

## Observing the Leak

### Using Observer

```erlang
observer:start().
```

Then:
1. Go to "Processes" tab
2. Find the `chat_room` process
3. Watch "Memory" column as images are sent
4. Notice it doesn't decrease even though room doesn't "store" images

### Using Process Info

```erlang
% Get the room PID
RoomPid = whereis('general').

% Check detailed memory
erlang:process_info(RoomPid, [memory, binary, message_queue_len]).

% The 'binary' field shows accumulated ProcBin references!
```

## Why This Happens

### The Binary Reference Chain

1. **Image arrives**: `handle_cast({image, FromName, ImageBinary}, State)`
   - Room receives message with large binary
   - Creates a ProcBin reference on room's heap

2. **Broadcast**: `broadcast(Members, {image, FromName, ImageBinary})`
   - Room sends same binary to all members
   - Each send creates another reference
   - Original reference stays in room's processed mailbox

3. **No GC trigger**:
   - Room does minimal work (just forwarding)
   - Doesn't allocate much on heap
   - GC doesn't run frequently enough

4. **References accumulate**:
   - Even "processed" messages keep references until GC
   - With slow GC, references pile up
   - Memory grows with each image sent

### Why Clients Don't Leak

Clients receive the images but:
- Process them (formatting, display logic)
- Do more heap allocation
- Trigger GC more frequently
- Clear references naturally

## The Fix

### Option 1: Force GC (Temporary)

```erlang
handle_cast({image, FromName, ImageBinary}, State) ->
    broadcast(Members, {image, FromName, ImageBinary}),

    % Force GC periodically
    case State#state.message_count rem 10 of
        0 -> erlang:garbage_collect();
        _ -> ok
    end,

    {noreply, State#state{message_count = Count + 1}}.
```

**Downside**: Still holds references briefly, GC overhead.

### Option 2: Store in ETS (Better)

```erlang
handle_cast({image, FromName, ImageBinary}, State) ->
    % Store in ETS instead of passing binary around
    ImageId = make_ref(),
    ets:insert(image_storage, {ImageId, ImageBinary}),

    % Send only reference
    broadcast(Members, {image_ref, FromName, ImageId}),

    {noreply, State#state{message_count = Count + 1}}.
```

**Clients then**:
```erlang
handle_info({image_ref, FromName, ImageId}, State) ->
    [{_, ImageBinary}] = ets:lookup(image_storage, ImageId),
    % Process image...
    {noreply, State}.
```

### Option 3: Separate Image Relay (Best)

Don't route images through chat room at all:
- Chat room only handles text messages
- Images go through separate binary-aware service
- That service can use ETS, disk storage, or external object store

## Learning Points

1. **Processes that relay data can leak** - Even without explicit storage
2. **Low-activity processes rarely GC** - Can accumulate references
3. **Large binaries need special handling** - Don't pass them in messages if possible
4. **Process memory != just State** - Includes mailbox, references, stack
5. **Observer is your friend** - Visualize the problem

## Exercise Questions

1. Why don't clients leak even though they receive the same images?
2. What would happen if you sent 1000 images through the room?
3. How would you modify the room to use ETS for image storage?
4. Could you use `binary:copy/1`? When would that help vs hurt?
5. What metrics would you monitor in production to detect this leak?

## For Instructors

This example is designed to:
- Show a realistic scenario (chat/image sharing)
- Demonstrate the non-obvious nature of binary leaks
- Teach the difference between "storing" and "holding references"
- Illustrate why process activity matters for GC
- Provide observable symptoms (memory growth, Observer visualization)
- Offer multiple solution approaches with trade-offs
