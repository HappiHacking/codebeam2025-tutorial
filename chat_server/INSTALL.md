# Installation and Setup

## Prerequisites

This chat server requires Erlang/OTP 24 or later.

### Installing Erlang

**Ubuntu/Debian:**
```bash
sudo apt-get update
sudo apt-get install erlang
```

**macOS (Homebrew):**
```bash
brew install erlang
```

**From source or other methods:**
Visit https://www.erlang.org/downloads

## Building the Application

Once Erlang is installed:

```bash
cd chat_server
make compile
```

Or manually:

```bash
mkdir -p ebin
erlc -o ebin src/*.erl
erlc -o ebin demo.erl
```

## Running the Demos

### Basic Demo
```bash
make run
```

Or:
```bash
erl -pa ebin -s demo run -s init stop
```

### Leak Demonstration
```bash
make demo
```

### Stress Test
```bash
make stress
```

### Interactive Shell
```bash
make shell
```

Then in the shell:
```erlang
application:start(chat_server).
demo:run().
```

## Verifying the Leak

The best way to see the leak is with Observer:

```bash
erl -pa ebin
```

Then:
```erlang
observer:start().
application:start(chat_server).
demo:demonstrate_leak().
```

Watch the `chat_room` process memory grow in the Processes tab!
