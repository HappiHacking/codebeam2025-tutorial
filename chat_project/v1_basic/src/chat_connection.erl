-module(chat_connection).
-export([handle/1, loop/2]).

%% Connection handler - one process per client
%% Uses process dictionary to store state (anti-pattern, fixed in v2)

handle(Socket) ->
    %% Set socket to active mode so we receive data as messages
    inet:setopts(Socket, [{active, true}]),

    %% Send welcome message
    send(Socket, "Welcome to Erlang Chat Server!\n"),
    send(Socket, "Commands:\n"),
    send(Socket, "  JOIN <room> <username> - Join a chat room\n"),
    send(Socket, "  MSG <message>          - Send a message\n"),
    send(Socket, "  LEAVE                  - Leave current room\n"),
    send(Socket, "  QUIT                   - Disconnect\n\n"),

    %% Initialize state in process dictionary (anti-pattern!)
    put(socket, Socket),
    put(room, undefined),
    put(username, undefined),

    %% Enter main loop
    loop(Socket, unauthenticated).

%% Main loop - handle both socket input and room messages
loop(Socket, State) ->
    receive
        %% Room messages
        {joined, Username, UserCount} ->
            Msg = io_lib:format("OK: Joined as ~s (~p users in room)\n",
                              [Username, UserCount]),
            send(Socket, Msg),
            loop(Socket, State);

        {message, Message} ->
            send(Socket, Message),
            loop(Socket, State);

        {left, _Username} ->
            send(Socket, "OK: Left room\n"),
            loop(Socket, State);

        %% Socket messages (active mode)
        {tcp, Socket, Data} ->
            Line = string:trim(binary_to_list(Data)),
            case parse_command(Line) of
                {join, Room, Username} when State =:= unauthenticated ->
                    handle_join(Socket, Room, Username),
                    loop(Socket, authenticated);

                {join, _Room, _Username} ->
                    send(Socket, "ERROR: Already in a room. LEAVE first.\n"),
                    loop(Socket, State);

                {msg, Message} when State =:= authenticated ->
                    handle_message(Socket, Message),
                    loop(Socket, State);

                {msg, _Message} ->
                    send(Socket, "ERROR: Join a room first.\n"),
                    loop(Socket, State);

                {leave} when State =:= authenticated ->
                    handle_leave(Socket),
                    loop(Socket, unauthenticated);

                {leave} ->
                    send(Socket, "ERROR: Not in a room.\n"),
                    loop(Socket, State);

                quit ->
                    handle_quit(Socket),
                    ok;

                {error, Reason} ->
                    send(Socket, io_lib:format("ERROR: ~s\n", [Reason])),
                    loop(Socket, State);

                unknown ->
                    send(Socket, "ERROR: Unknown command\n"),
                    loop(Socket, State)
            end;

        {tcp_closed, Socket} ->
            cleanup(Socket),
            ok;

        {tcp_error, Socket, _Reason} ->
            cleanup(Socket),
            ok
    end.

%% Command parsing
parse_command(Line) ->
    Tokens = string:tokens(Line, " "),
    case Tokens of
        ["JOIN", Room, Username] ->
            {join, list_to_atom(Room), Username};
        ["JOIN" | _] ->
            {error, "JOIN requires: JOIN <room> <username>"};

        ["MSG" | MessageWords] ->
            Message = string:join(MessageWords, " "),
            {msg, Message};

        ["LEAVE"] ->
            {leave};

        ["QUIT"] ->
            quit;

        [] ->
            unknown;

        _ ->
            unknown
    end.

%% Command handlers

handle_join(_Socket, Room, Username) ->
    chat_room:join(Room, {Username, self()}),
    put(room, Room),
    put(username, Username).

handle_message(Socket, Message) ->
    Room = get(room),
    case Room of
        undefined ->
            send(Socket, "ERROR: Not in a room\n");
        _ ->
            chat_room:broadcast(Room, self(), Message ++ "\n")
    end.

handle_leave(_Socket) ->
    Room = get(room),
    case Room of
        undefined ->
            ok;
        _ ->
            chat_room:leave(Room, self()),
            put(room, undefined),
            put(username, undefined)
    end.

handle_quit(Socket) ->
    handle_leave(Socket),
    send(Socket, "Goodbye!\n"),
    gen_tcp:close(Socket).

cleanup(Socket) ->
    handle_leave(Socket),
    gen_tcp:close(Socket).

%% Helper to send data
send(Socket, Data) ->
    gen_tcp:send(Socket, Data).
