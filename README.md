# CodeBeam 2025 BEAM Tutorial

## Files

- `livebooks/`: Livebook exercises (see below)
- `chat_project/`: Chat server workshop project, with first step given :)
- `chat_server/`: Alternative chat server project
- `code_examples/`: other project ideas
- `pdf/`: Slide handouts

## Livebook Installation

### Docker

```shell
# Ready-made for you - just run in cloned repo root :)
docker run -p 8080:8080 -p 8081:8081 --pull always -e LIVEBOOK_PASSWORD="codebeam2025" -u $(id -u):$(id -g) -v $(pwd)/livebooks:/data ghcr.io/livebook-dev/livebook
```
In order to access and save notebooks directly to your machine
you can mount a local directory into the container.
Make sure to specify the user with "-u $(id -u):$(id -g)"
so that the created files have proper permissions

```shell
# Running with the default configuration
docker run -p 8080:8080 -p 8081:8081 --pull always ghcr.io/livebook-dev/livebook
```
You can configure Livebook using environment variables,
for all options see the dedicated "Environment variables" section below
```shell
docker run -p 8080:8080 -p 8081:8081 --pull always -e LIVEBOOK_PASSWORD="codebeam2025" ghcr.io/livebook-dev/livebook
```

Or if you need to run on different ports:
```shell
docker run -p 8090:8090 -p 8091:8091 --pull always -e LIVEBOOK_PORT=8090 -e LIVEBOOK_IFRAME_PORT=8091 ghcr.io/livebook-dev/livebook
```
### Desktop app

  * [Download the installer for Mac and Windows from our homepage](https://livebook.dev/#install)

  * Latest stable builds: [Mac (Universal)](https://github.com/livebook-dev/livebook/releases/latest/download/LivebookInstall-macos-universal.dmg),
    [Windows](https://github.com/livebook-dev/livebook/releases/latest/download/LivebookInstall-windows-x86_64.exe)

  * Nightly builds: [Mac (Universal)](https://github.com/livebook-dev/livebook/releases/download/nightly/LivebookInstall-macos-universal.dmg),
    [Windows](https://github.com/livebook-dev/livebook/releases/download/nightly/LivebookInstall-windows-x86_64.exe)

  * Builds for particular Livebook version are available on our
    [GitHub releases](https://github.com/livebook-dev/livebook/releases).

### Direct installation with Elixir

You can run Livebook on your own machine using just Elixir. You will need
[Elixir v1.18](https://elixir-lang.org/install.html) or later.
Livebook also requires the following Erlang applications: `inets`,
`os_mon`, `runtime_tools`, `ssl` and `xmerl`. Those applications come
with most Erlang distributions but certain package managers may split
them apart. For example, on Ubuntu, these Erlang applications can
be installed as follows:

```shell
sudo apt install erlang-inets erlang-os-mon erlang-runtime-tools erlang-ssl erlang-xmerl erlang-dev erlang-parsetools
```

**Note:** The [`livebook` package](https://hex.pm/packages/livebook)
is meant to be used as a CLI tool. Livebook is not officially
supported as a Mix/Hex dependency.

#### Escript

Running Livebook using Escript makes for a very convenient option
for local usage and provides easy configuration via CLI options.

```shell
mix do local.rebar --force, local.hex --force
mix escript.install hex livebook

# Start the Livebook server
livebook server

# See all the configuration options
livebook server --help
```

After you install the escript, make sure you add the directory where
Elixir keeps escripts to your [$PATH](https://en.wikipedia.org/wiki/PATH_(variable)).
If you installed Elixir with `asdf`, you'll need to run `asdf reshim elixir`
once the escript is built.

To try out features from the main branch you can alternatively
install the escript directly from GitHub like this:

```shell
mix escript.install github livebook-dev/livebook
```