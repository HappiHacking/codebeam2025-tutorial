-module(chat_server_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("Starting Chat Server...~n"),
    chat_server_sup:start_link().

stop(_State) ->
    ok.
