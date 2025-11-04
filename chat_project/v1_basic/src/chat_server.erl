-module(chat_server).
-export([start/0, start/1, acceptor_loop/1]).

%% Simple TCP chat server
%% Usage: chat_server:start(Port)
%% Connect with: telnet localhost Port

-define(DEFAULT_PORT, 5555).

%% Start server on default port
start() ->
    start(?DEFAULT_PORT).

%% Start server on specific port
start(Port) ->
    %% Start TCP listener
    case gen_tcp:listen(Port, [binary, {packet, line}, {active, false}, {reuseaddr, true}]) of
        {ok, LSock} ->
            io:format("Chat server listening on port ~p~n", [Port]),
            io:format("Connect with: telnet localhost ~p~n", [Port]),
            spawn(?MODULE, acceptor_loop, [LSock]);
        {error, Reason} ->
            io:format("Failed to start server: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Accept connections in a loop
acceptor_loop(LSock) ->
    case gen_tcp:accept(LSock) of
        {ok, Socket} ->
            %% Spawn a connection handler
            Pid = spawn(chat_connection, handle, [Socket]),
            gen_tcp:controlling_process(Socket, Pid),
            io:format("New connection from ~p~n", [Pid]),
            acceptor_loop(LSock);
        {error, Reason} ->
            io:format("Accept error: ~p~n", [Reason]),
            acceptor_loop(LSock)
    end.
