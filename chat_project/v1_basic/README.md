# Chat Server v1: Basic Implementation

Simple TCP-based chat server using only concepts from Chapters 1-7.

## Features

- TCP connections (telnet-compatible)
- Multiple chat rooms
- Simple text protocol
- Process-per-connection architecture
- Room processes with registered names

## Architecture

```
chat_server (acceptor)
    |
    ├─> chat_connection (client 1) ──┐
    ├─> chat_connection (client 2) ──┼──> chat_room (lobby)
    └─> chat_connection (client 3) ──┘
```

**Processes:**
- `chat_server`: TCP acceptor, spawns connection handlers
- `chat_connection`: One per client, handles commands
- `chat_room`: One per room, manages users and broadcasts

## Building

```bash
cd code/projects/chat/v1_basic
make
```

Or manually:

```bash
erlc -o ebin src/*.erl
```

## Running

```bash
# Start server (default port 5555)
erl -pa ebin
1> chat_server:start().

# Or specific port
1> chat_server:start(8000).
```

## Testing

Connect with telnet from multiple terminals:

```bash
# Terminal 1
telnet localhost 5555
JOIN lobby alice
MSG Hello everyone!
LEAVE
QUIT

# Terminal 2
telnet localhost 5555
JOIN lobby bob
MSG Hi Alice!
```

## Protocol

```
Client                    Server
------                    ------
JOIN <room> <username> -> OK: Joined as <username> (N users in room)
                       <- *** <username> joined the room

MSG <message text>     -> (no response)
                       <- [<username>] <message text>  (to all others)

LEAVE                  -> OK: Left room
                       <- *** <username> left the room (to all others)

QUIT                   -> Goodbye!
                       <- (connection closed)
```

## Known Limitations (Fixed in Later Versions)

### v1_basic Issues:

1. **Process dictionary for state** (anti-pattern)
   ```erlang
   put(room, RoomName)  % Bad: hidden state
   get(room)            % Bad: implicit dependencies
   ```
   **Fixed in v2_domains:** Use explicit state records

2. **Naive broadcast** (floods all processes)
   ```erlang
   lists:foreach(fun({_, Pid}) -> Pid ! {message, Msg} end, Users)
   ```
   **Fixed in v3_flows:** Add backpressure and batching

3. **No supervision** (crashed processes disappear)
   ```erlang
   spawn(chat_connection, handle, [Socket])  % No monitoring
   ```
   **Fixed in v4_otp:** Proper supervision trees

4. **Manual process registration**
   ```erlang
   register(RoomName, Pid)  % Race conditions possible
   ```
   **Fixed in v4_otp:** Use OTP registry or gproc

5. **Blocking receive with timeout**
   ```erlang
   gen_tcp:recv(Socket, 0)  % Blocks for 60s
   after 60000 -> check_messages()
   ```
   **Fixed in v1.5:** Use {active, once} for better responsiveness

## File Structure

```
v1_basic/
├── src/
│   ├── chat_server.erl       # TCP acceptor
│   ├── chat_room.erl          # Room process
│   └── chat_connection.erl    # Connection handler
├── ebin/                      # Compiled .beam files
├── Makefile
└── README.md
```

## Code Walkthrough

### chat_server.erl

TCP acceptor using `gen_tcp:listen/2` and `gen_tcp:accept/1`:

```erlang
acceptor_loop(LSock) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    Pid = spawn(chat_connection, handle, [Socket]),
    gen_tcp:controlling_process(Socket, Pid),
    acceptor_loop(LSock).
```

### chat_room.erl

Room process maintains list of users:

```erlang
loop(Users) ->
    receive
        {join, Username, Pid} ->
            NewUsers = [{Username, Pid} | Users],
            broadcast_to_others("*** joined", Pid, Users),
            loop(NewUsers);
        {broadcast, FromPid, Msg} ->
            broadcast_to_others(Msg, FromPid, Users),
            loop(Users)
    end.
```

### chat_connection.erl

Connection handler parses commands:

```erlang
loop(Socket, State) ->
    {ok, Data} = gen_tcp:recv(Socket, 0),
    case parse_command(Data) of
        {join, Room, Username} ->
            chat_room:join(Room, {Username, self()}),
            loop(Socket, authenticated);
        {msg, Message} ->
            chat_room:broadcast(get(room), self(), Message),
            loop(Socket, State)
    end.
```

## What This Demonstrates

From **Chapter 1-7 concepts:**
- ✅ Spawning processes (`spawn/3`)
- ✅ Message passing (`!` and `receive`)
- ✅ Process registration (`register/2`, `whereis/1`)
- ✅ Pattern matching in receive
- ✅ TCP sockets (`gen_tcp`)
- ✅ Binary/list conversions
- ✅ Recursion for loops

**Not yet demonstrated** (later chapters):
- ❌ Domain modeling (Part II)
- ❌ Flow control (Part III)
- ❌ OTP behaviors (Part V)
- ❌ Supervision (Part V)

## Next Steps

See how this code evolves:
- **v2_domains**: Clean domain separation (Chapter 13)
- **v3_flows**: Backpressure and protocols (Chapter 19)
- **v4_otp**: Production OTP (Chapter 33)

## Exercises

1. Add a `/users` command to list users in current room
2. Add a `/rooms` command to list all active rooms
3. Add private messaging: `PRIVMSG <username> <message>`
4. Add a maximum room size limit
5. Add timestamps to messages
6. Make it crash-safe: what happens if a connection dies mid-message?
