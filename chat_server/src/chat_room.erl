-module(chat_room).
-behaviour(gen_server).

%% API
-export([start_link/1, join/2, leave/2, send_message/3, send_image/3, get_stats/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    room_name :: binary(),
    members = [] :: [{pid(), binary()}],  % List of {ClientPid, ClientName}
    message_count = 0 :: integer()
    % NOTE: No separate storage for images - they stay in messages!
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(RoomName) ->
    gen_server:start_link({local, binary_to_atom(RoomName)}, ?MODULE, [RoomName], []).

join(RoomName, ClientPid) ->
    gen_server:call(binary_to_atom(RoomName), {join, ClientPid}).

leave(RoomName, ClientPid) ->
    gen_server:cast(binary_to_atom(RoomName), {leave, ClientPid}).

send_message(RoomName, FromName, Message) ->
    gen_server:cast(binary_to_atom(RoomName), {message, FromName, Message}).

send_image(RoomName, FromName, ImageBinary) ->
    gen_server:cast(binary_to_atom(RoomName), {image, FromName, ImageBinary}).

get_stats(RoomName) ->
    gen_server:call(binary_to_atom(RoomName), get_stats).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([RoomName]) ->
    io:format("Chat room ~s started~n", [RoomName]),
    {ok, #state{room_name = RoomName}}.

handle_call({join, ClientPid}, _From, State = #state{members = Members, room_name = RoomName}) ->
    % Monitor the client
    erlang:monitor(process, ClientPid),

    % Get client name
    ClientName = gen_server:call(ClientPid, get_name),

    NewMembers = [{ClientPid, ClientName} | Members],

    io:format("~s joined room ~s~n", [ClientName, RoomName]),

    % Notify all members about new joiner
    broadcast(NewMembers, {user_joined, ClientName}),

    {reply, ok, State#state{members = NewMembers}};

handle_call(get_stats, _From, State = #state{room_name = RoomName,
                                               members = Members,
                                               message_count = Count}) ->
    {memory, Memory} = erlang:process_info(self(), memory),
    {message_queue_len, QueueLen} = erlang:process_info(self(), message_queue_len),

    Stats = #{
        room_name => RoomName,
        member_count => length(Members),
        message_count => Count,
        memory_bytes => Memory,
        queue_length => QueueLen
    },

    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({message, FromName, Message}, State = #state{members = Members,
                                                          message_count = Count}) ->
    % THIS IS THE PROBLEM: We broadcast the message which means the room process
    % holds a reference to the message in its mailbox until it's processed.
    % For text messages this is fine, but for images...
    broadcast(Members, {message, FromName, Message}),

    % Room does minimal work, so GC rarely runs!
    % The message stays in the mailbox history even after being processed
    {noreply, State#state{message_count = Count + 1}};

handle_cast({image, FromName, ImageBinary}, State = #state{members = Members,
                                                            message_count = Count}) ->
    % THIS DEMONSTRATES THE LEAK:
    % 1. Large binary comes in as a message
    % 2. We broadcast it to all members (more references!)
    % 3. Room process does very little work, no GC trigger
    % 4. Binary stays alive in room's heap via ProcBin reference
    % 5. Even after broadcast, the reference in our processed mailbox keeps it alive

    ImageSize = byte_size(ImageBinary),
    io:format("Room broadcasting image from ~s (~p bytes)~n", [FromName, ImageSize]),

    broadcast(Members, {image, FromName, ImageBinary}),

    % Even though we don't store the image explicitly, the room process
    % holds references through:
    % - The received message (until GC runs)
    % - The broadcast messages in our own mailbox if members are slow

    {noreply, State#state{message_count = Count + 1}};

handle_cast({leave, ClientPid}, State = #state{members = Members, room_name = RoomName}) ->
    case lists:keyfind(ClientPid, 1, Members) of
        {ClientPid, ClientName} ->
            io:format("~s left room ~s~n", [ClientName, RoomName]),
            NewMembers = lists:keydelete(ClientPid, 1, Members),
            broadcast(NewMembers, {user_left, ClientName}),
            {noreply, State#state{members = NewMembers}};
        false ->
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, ClientPid, _Reason}, State = #state{members = Members}) ->
    % Client crashed or disconnected
    case lists:keyfind(ClientPid, 1, Members) of
        {ClientPid, ClientName} ->
            io:format("~s disconnected (process down)~n", [ClientName]),
            NewMembers = lists:keydelete(ClientPid, 1, Members),
            broadcast(NewMembers, {user_left, ClientName}),
            {noreply, State#state{members = NewMembers}};
        false ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{room_name = RoomName}) ->
    io:format("Chat room ~s terminating~n", [RoomName]),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

broadcast(Members, Message) ->
    lists:foreach(fun({Pid, _Name}) ->
        Pid ! Message
    end, Members).
