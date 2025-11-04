-module(test_client).
-compile(export_all).

%% Simple test client for the chat server

test_join() ->
    {ok, Socket} = gen_tcp:connect("localhost", 5555, [binary, {packet, line}, {active, false}]),

    % Read welcome message
    io:format("=== Connecting ===~n"),
    read_until_prompt(Socket),

    % Send JOIN command
    io:format("~n=== Sending JOIN lobby alice ===~n"),
    ok = gen_tcp:send(Socket, "JOIN lobby alice\n"),
    timer:sleep(100),
    {ok, Response} = gen_tcp:recv(Socket, 0, 1000),
    io:format("Response: ~s", [Response]),

    % Send a message
    io:format("~n=== Sending MSG Hello! ===~n"),
    ok = gen_tcp:send(Socket, "MSG Hello!\n"),
    timer:sleep(100),

    % Quit
    io:format("~n=== Sending QUIT ===~n"),
    ok = gen_tcp:send(Socket, "QUIT\n"),
    timer:sleep(100),
    {ok, Goodbye} = gen_tcp:recv(Socket, 0, 1000),
    io:format("Response: ~s", [Goodbye]),

    gen_tcp:close(Socket),
    ok.

read_until_prompt(Socket) ->
    case gen_tcp:recv(Socket, 0, 1000) of
        {ok, Line} ->
            io:format("~s", [Line]),
            read_until_prompt(Socket);
        {error, timeout} ->
            ok
    end.

test_two_users() ->
    % Start first client
    {ok, S1} = gen_tcp:connect("localhost", 5555, [binary, {packet, line}, {active, false}]),
    {ok, S2} = gen_tcp:connect("localhost", 5555, [binary, {packet, line}, {active, false}]),

    % Read welcomes
    read_until_prompt(S1),
    read_until_prompt(S2),

    io:format("~n=== Alice joins lobby ===~n"),
    gen_tcp:send(S1, "JOIN lobby alice\n"),
    timer:sleep(100),
    {ok, R1} = gen_tcp:recv(S1, 0, 1000),
    io:format("Alice got: ~s", [R1]),

    io:format("~n=== Bob joins lobby ===~n"),
    gen_tcp:send(S2, "JOIN lobby bob\n"),
    timer:sleep(100),
    {ok, R2} = gen_tcp:recv(S2, 0, 1000),
    io:format("Bob got: ~s", [R2]),

    % Check if Alice got notification of Bob joining
    case gen_tcp:recv(S1, 0, 1000) of
        {ok, Notif} ->
            io:format("Alice got notification: ~s", [Notif]);
        {error, timeout} ->
            io:format("Alice didn't get notification (might be a bug)~n")
    end,

    io:format("~n=== Alice sends message ===~n"),
    gen_tcp:send(S1, "MSG Hello from Alice!\n"),
    timer:sleep(100),

    % Check if Bob got the message
    case gen_tcp:recv(S2, 0, 1000) of
        {ok, Msg} ->
            io:format("Bob got: ~s", [Msg]);
        {error, timeout} ->
            io:format("Bob didn't get message (might be a bug)~n")
    end,

    gen_tcp:send(S1, "QUIT\n"),
    gen_tcp:send(S2, "QUIT\n"),
    gen_tcp:close(S1),
    gen_tcp:close(S2),
    ok.
