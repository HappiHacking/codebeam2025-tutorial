-module(chat_room).
-export([start/1, loop/1, join/2, leave/2, broadcast/3]).

%% Room process - manages users in a chat room
%% State: list of {Username, ConnectionPid} tuples

%% Start a room process and register it
start(RoomName) ->
    case whereis(RoomName) of
        undefined ->
            Pid = spawn(?MODULE, loop, [[]]),
            register(RoomName, Pid),
            {ok, Pid};
        Pid ->
            {ok, Pid}  % Room already exists
    end.

%% Main loop - maintains list of users
loop(Users) ->
    receive
        {join, Username, ConnectionPid} ->
            %% Add user to room
            NewUsers = [{Username, ConnectionPid} | Users],
            ConnectionPid ! {joined, Username, length(NewUsers)},
            %% Notify others
            Msg = io_lib:format("*** ~s joined the room~n", [Username]),
            broadcast_to_others(Msg, ConnectionPid, Users),
            loop(NewUsers);

        {leave, ConnectionPid} ->
            %% Remove user from room
            case lists:keyfind(ConnectionPid, 2, Users) of
                {Username, ConnectionPid} ->
                    NewUsers = lists:keydelete(ConnectionPid, 2, Users),
                    Msg = io_lib:format("*** ~s left the room~n", [Username]),
                    broadcast_to_all(Msg, NewUsers),
                    ConnectionPid ! {left, Username},
                    loop(NewUsers);
                false ->
                    loop(Users)
            end;

        {broadcast, FromPid, Message} ->
            %% Broadcast message from a user
            case lists:keyfind(FromPid, 2, Users) of
                {Username, FromPid} ->
                    Msg = io_lib:format("[~s] ~s", [Username, Message]),
                    broadcast_to_others(Msg, FromPid, Users);
                false ->
                    ok
            end,
            loop(Users);

        {list_users, FromPid} ->
            %% Send list of users to requester
            Usernames = [U || {U, _} <- Users],
            FromPid ! {users, Usernames},
            loop(Users);

        stop ->
            ok;

        _Other ->
            loop(Users)
    end.

%% API functions

join(RoomName, {Username, ConnectionPid}) ->
    %% Ensure room exists
    {ok, _RoomPid} = start(RoomName),
    RoomName ! {join, Username, ConnectionPid}.

leave(RoomName, ConnectionPid) ->
    case whereis(RoomName) of
        undefined -> ok;
        _Pid -> RoomName ! {leave, ConnectionPid}
    end.

broadcast(RoomName, FromPid, Message) ->
    case whereis(RoomName) of
        undefined -> {error, no_such_room};
        _Pid -> RoomName ! {broadcast, FromPid, Message}
    end.

%% Internal helper functions

broadcast_to_all(Message, Users) ->
    lists:foreach(fun({_Username, Pid}) ->
        Pid ! {message, Message}
    end, Users).

broadcast_to_others(Message, FromPid, Users) ->
    lists:foreach(fun({_Username, Pid}) ->
        if Pid =/= FromPid ->
            Pid ! {message, Message};
        true ->
            ok
        end
    end, Users).
