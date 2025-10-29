# codebeam2025-tutorial

## Installation

We provide several methods for running Livebook,
pick the one that best fits your use case.

### Desktop app

  * [Download the installer for Mac and Windows from our homepage](https://livebook.dev/#install)

  * Latest stable builds: [Mac (Universal)](https://github.com/livebook-dev/livebook/releases/latest/download/LivebookInstall-macos-universal.dmg),
    [Windows](https://github.com/livebook-dev/livebook/releases/latest/download/LivebookInstall-windows-x86_64.exe)

  * Nightly builds: [Mac (Universal)](https://github.com/livebook-dev/livebook/releases/download/nightly/LivebookInstall-macos-universal.dmg),
    [Windows](https://github.com/livebook-dev/livebook/releases/download/nightly/LivebookInstall-windows-x86_64.exe)

  * Builds for particular Livebook version are available on our
    [GitHub releases](https://github.com/livebook-dev/livebook/releases).

### Docker

Running Livebook using Docker is another great option to run Livebook
in case you don't have Elixir installed.

```shell
# Running with the default configuration
docker run -p 8080:8080 -p 8081:8081 --pull always ghcr.io/livebook-dev/livebook

# In order to access and save notebooks directly to your machine
# you can mount a local directory into the container.
# Make sure to specify the user with "-u $(id -u):$(id -g)"
# so that the created files have proper permissions
docker run -p 8080:8080 -p 8081:8081 --pull always -u $(id -u):$(id -g) -v $(pwd):/data ghcr.io/livebook-dev/livebook

# You can configure Livebook using environment variables,
# for all options see the dedicated "Environment variables" section below
docker run -p 8080:8080 -p 8081:8081 --pull always -e LIVEBOOK_PASSWORD="securesecret" ghcr.io/livebook-dev/livebook

# Or if you need to run on different ports:
docker run -p 8090:8090 -p 8091:8091 --pull always -e LIVEBOOK_PORT=8090 -e LIVEBOOK_IFRAME_PORT=8091 ghcr.io/livebook-dev/livebook
```
