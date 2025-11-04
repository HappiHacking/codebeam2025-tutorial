# Teaching Notes: Chat Server Binary Leak Demo

## Purpose

This application demonstrates a **realistic binary leak pattern** that experienced Erlang/Elixir developers can encounter in production systems. It's designed for Module 7 of the BEAM tutorial.

## The Core Problem

**Processes that relay messages can accumulate binary references even without explicitly storing them.**

### Why This Is Non-Obvious

1. **The room doesn't "store" images** - No explicit state field for images
2. **The code looks clean** - Just forwarding messages
3. **It works fine initially** - Leak accumulates slowly
4. **Observer shows the problem** - But only if you know to look

## Architecture Highlights

### Process Structure

```
User Code
    |
    v
chat_client (gen_server)
    |
    | send_image/2
    v
chat_room (gen_server) <--- THE LEAK IS HERE
    |
    | broadcast to all members
    v
chat_client processes (receive images)
```

### Why Room Leaks But Clients Don't

**Room Process:**
- Receives large binary in message
- Broadcasts to N members (creates N+1 references)
- Does MINIMAL work (just forwarding)
- GC rarely triggers
- ProcBin references accumulate

**Client Processes:**
- Receive binary
- Do REAL work (formatting, displaying, logging)
- Allocate more on heap
- GC triggers frequently
- References naturally cleaned up

## Key Teaching Moments

### 1. The Broadcast Pattern Is Common

This exact pattern appears in:
- Chat/messaging systems
- Pub/sub brokers
- Event distribution
- Log forwarding
- Metrics collection

### 2. "Doing Less" Can Be Bad

Counter-intuitive: The room process is "efficient" (minimal work) but this prevents GC from running, causing the leak.

### 3. Process Activity Matters

- High-activity processes: GC runs often
- Low-activity processes: Can accumulate references
- **It's not just about state, it's about mailbox and references**

### 4. Observer Is Essential

Students should:
1. Start observer before running demo
2. Find the `chat_room` process
3. Watch memory column as images are sent
4. Compare with client process memory (stays low)

## Demonstration Flow

### Phase 1: Normal Operation (5 min)

```erlang
application:start(chat_server).
{ok, _} = chat_server_sup:start_room(<<"demo">>).
{ok, Alice} = chat_server_sup:start_client(<<"Alice">>, <<"demo">>).
{ok, Bob} = chat_server_sup:start_client(<<"Bob">>, <<"demo">>).

% Text messages - no problem
chat_client:send_message(Alice, <<"Hello!">>).
chat_room:get_stats(<<"demo">>).  % Memory is low
```

### Phase 2: Image Sending (5 min)

```erlang
% Send one image
Image = crypto:strong_rand_bytes(1024 * 1024).
chat_client:send_image(Alice, Image).

% Check stats - memory increased!
chat_room:get_stats(<<"demo">>).
```

### Phase 3: Accumulation (5 min)

```erlang
% Send many images
[chat_client:send_image(Bob, crypto:strong_rand_bytes(1024*1024))
 || _ <- lists:seq(1, 10)].

% Memory keeps growing
chat_room:get_stats(<<"demo">>).

% Check binary references
RoomPid = whereis('demo'),
erlang:process_info(RoomPid, [memory, binary, message_queue_len]).
```

## Solutions (For Discussion)

### Solution 1: Force GC (Temporary Fix)

```erlang
% In handle_cast for images:
case State#state.message_count rem 10 of
    0 -> erlang:garbage_collect();
    _ -> ok
end
```

**Pros:** Simple, works
**Cons:** Still holds references briefly, GC overhead

### Solution 2: Store in ETS (Better)

```erlang
% Room stores image in ETS, sends reference
ImageId = make_ref(),
ets:insert(image_storage, {ImageId, ImageBinary}),
broadcast(Members, {image_ref, FromName, ImageId})
```

**Pros:** Room never holds large binary
**Cons:** Need cleanup strategy, ETS management

### Solution 3: Separate Binary Service (Best)

```erlang
% Don't route binaries through chat at all
% Use separate image_relay process or external storage
% Chat room only routes text and references
```

**Pros:** Clean separation, scalable
**Cons:** More complex architecture

## Exercise Ideas

### For Students

1. **Measure the leak**
   - How many MB after 10 images? 20?
   - What's the growth rate?
   - Does it ever stop growing?

2. **Fix it**
   - Implement Solution 1 (force GC)
   - Measure improvement
   - What's the cost?

3. **Implement Solution 2**
   - Add ETS table for images
   - Modify room to store there
   - Modify clients to fetch from ETS
   - Add cleanup policy (TTL, LRU)

4. **Production Detection**
   - What metrics would catch this?
   - How would you monitor in production?
   - What alerts would you set?

### For Instructors

1. **Before starting**: Show system_info(binary) before/after
2. **Use observer live**: Demo is more effective with visualization
3. **Compare processes**: Show client memory stays low while room grows
4. **Discuss trade-offs**: Each solution has costs, no perfect answer
5. **Real examples**: Mention WhatsApp, Discord, RabbitMQ patterns

## Common Student Questions

### "Why doesn't the room just store images in state?"

It doesn't need to! The leak happens because binaries pass through the mailbox. Even "processed" messages keep references until GC.

### "Won't GC eventually run?"

Yes, but "eventually" might be too late. In production, a room handling 1000 images before GC runs = memory exhaustion.

### "Why not just use smaller images?"

This is about the pattern, not the size. Even 100KB images leak if sent frequently enough.

### "Is this a BEAM bug?"

No! This is correct behavior. Binaries are reference-counted for efficiency. The "bug" is in how we architect the system.

## Production Relevance

### Systems Where This Happens

1. **Message brokers** - RabbitMQ-like systems
2. **API gateways** - Proxying requests with large payloads
3. **Log aggregators** - Forwarding log chunks
4. **Media servers** - Streaming or relaying media
5. **File upload services** - Relaying file chunks

### Detection in Production

Monitor:
- Process memory (`process_info(Pid, memory)`)
- Binary memory (`process_info(Pid, binary)`)
- Message queue length
- System-wide binary memory (`erlang:memory(binary)`)

Alert when:
- Process memory grows monotonically
- Binary count grows without bound
- Queue length increases steadily

## Integration with Module 7

This demo ties together:
- **Module 2**: Binary handling and copying costs
- **Module 3**: GC behavior and memory management
- **Module 6**: Anti-patterns and proper process design
- **Module 7**: Real-world debugging and optimization

## Success Criteria

Students understand:
1. ✓ Processes can leak without explicit storage
2. ✓ Low-activity processes are vulnerable
3. ✓ Observer is critical for debugging
4. ✓ Multiple valid solutions with trade-offs
5. ✓ This pattern is common in production
