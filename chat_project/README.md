# Chat Server - Project Evolution

WebSocket-based chat server demonstrating Erlang concurrency, message passing, and progressive refinement from basic processes to production OTP applications.

## Project Versions

| Version | Chapter/Part | Focus | Status |
|---------|--------------|-------|--------|
| **v1_basic** | Chapter 26 | Raw processes, basic concepts | Planning |
| **v2_domains** | Part III (Ch 7-9) | Domain modeling | Planned |
| **v3_flows** | Part IV (Ch 10-13) | Flow protocols, backpressure | Planned |
| **v4_otp** | Part VI (Ch 22-25) | OTP behaviors, supervision | Planned |

## Concepts Demonstrated

### v1_basic: Chat Over WebSockets (Chapter 26)

**Core Erlang concepts:**
- Process-per-connection model (`spawn`)
- Message passing (`!` and `receive`)
- Pattern matching in message handlers
- Process lifecycle and termination
- Simple process registration

**Application features:**
- WebSocket upgrade via Cowboy
- Multiple chat rooms
- User presence tracking
- Message broadcast
- Basic backpressure (naive)

**Archetypes introduced (naive):**
- **Resource Owner**: Connection process owns WebSocket
- **Router**: Room process routes messages to connections
- **Supervisor**: Manual process monitoring (crashes not recovered)

**Known limitations (addressed in later versions):**
- No supervision → crashed processes disappear
- Process dictionary for state → anti-pattern
- Naive broadcast → floods all processes
- No flow control → overload possible
- Manual registration → race conditions

**Learning goal:** "This works, but we can do better."

---

### v2_domains: Domain Modeling Applied (Part III)

**Refactors from v1:**
- Replace maps/process-dictionary with domain types
- Separate `user`, `room`, `connection` domains
- Type specs for all domain data
- Clear ownership boundaries

**New concepts:**
- Domain-driven design in Erlang
- Records vs. maps for domain entities
- Type specifications for documentation
- Domain boundary identification

**What improves:**
- Code clarity (what is this data?)
- Type safety (can't mix user IDs and room IDs)
- Testability (domain logic separate from I/O)

**Still to improve:**
- Flow control still naive
- No supervision
- Registration still manual

---

### v3_flows: Flow Protocols & Backpressure (Part IV)

**Refactors from v2:**
- Define explicit message protocols
- Add backpressure control loops
- Implement message batching
- Clear ownership semantics

**New concepts:**
- Protocol design (who sends what to whom)
- Flow control (credit-based or window-based)
- Batching for efficiency
- Message ownership transfer

**What improves:**
- No overload under heavy traffic
- Predictable performance
- Clear protocol documentation

**Still to improve:**
- No supervision (still crashes)
- Manual state machines

---

### v4_otp: Production OTP Implementation (Part VI)

**Refactors from v3:**
- Connection process → `gen_server`
- Room process → `gen_server`
- Manual supervision → `supervisor` trees
- Application structure with `.app` file

**New concepts:**
- `gen_server` behavior
- `supervisor` strategies (one_for_one, rest_for_one)
- OTP application structure
- Release management

**What improves:**
- Automatic crash recovery
- Standard OTP patterns
- Production deployment ready
- Tooling integration (observer, recon)

**Production ready:** Yes

---

## Running Each Version

### v1_basic

```bash
cd code/projects/chat/v1_basic
make deps    # Download Cowboy
make compile
make run

# In another terminal
wscat -c ws://localhost:8080/chat
```

### v2_domains

```bash
cd code/projects/chat/v2_domains
make compile
make run
```

Same interface as v1, improved internals.

### v3_flows

```bash
cd code/projects/chat/v3_flows
make compile
make run
```

Same interface, handles load better.

### v4_otp

```bash
cd code/projects/chat/v4_otp
make release
_build/default/rel/chat/bin/chat foreground
```

Production deployment.

## Testing Each Version

Each version has comprehensive tests:

```bash
cd code/projects/chat/v1_basic
make test

# Load testing (all versions)
make load-test
```

Tests verify:
- Functional correctness (all versions)
- Performance characteristics (compare versions)
- Error handling improvements

## Cross-References

**Chapter 26** introduces v1_basic with explanation of:
- Why process-per-connection works
- How message passing implements chat
- What problems we'll solve later

**Part III (Domains)** revisits with v2:
- Chapter 7: Domain discovery in chat
- Chapter 8: Data structure design
- Chapter 9: Heuristics for boundaries

**Part IV (Flows)** revisits with v3:
- Chapter 10: Ownership (socket, room, user)
- Chapter 11: Process structuring
- Chapter 12: Error handling flows
- Chapter 13: Protocol design

**Part V (Archetypes)** references chat as example:
- Chapter 14: Router (room routing)
- Chapter 15: Resource Owner (connection)
- Chapter 18: Supervisor (monitoring)

**Part VI (OTP)** revisits with v4:
- Chapter 22: gen_server pattern
- Chapter 23: Supervisors
- Chapter 24: Applications
- Chapter 25: Releases

## Evolution Summary

```
v1_basic: Works, shows concepts
    ↓
v2_domains: Clearer, typed, testable
    ↓
v3_flows: Performant, robust protocols
    ↓
v4_otp: Production-ready, standard patterns
```

Each version runs independently. The progression shows **why** improvements matter by demonstrating the problems they solve.

## Files

Each version directory contains:
- `src/` - Source code
- `test/` - Tests
- `Makefile` - Build commands
- `README.md` - Version-specific notes
- `rebar.config` - Dependencies (if any)

## Key Insight

The chat server isn't just an example. It's a **teaching vehicle** that:

1. Introduces concepts in simple form (v1)
2. Shows how domain thinking improves code (v2)
3. Demonstrates protocol design importance (v3)
4. Reveals why OTP patterns exist (v4)

By the end, readers understand not just **how** to write OTP code, but **why** the patterns evolved.
