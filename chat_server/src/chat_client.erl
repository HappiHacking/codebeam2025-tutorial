-module(chat_client).
-behaviour(gen_server).

%% API
-export([start_link/2, send_message/2, send_image/2, get_messages/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    name :: binary(),
    room_name :: binary(),
    messages = [] :: list()  % Store received messages
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ClientName, RoomName) ->
    gen_server:start_link(?MODULE, [ClientName, RoomName], []).

send_message(ClientPid, Message) ->
    gen_server:cast(ClientPid, {send_message, Message}).

send_image(ClientPid, ImageBinary) ->
    gen_server:cast(ClientPid, {send_image, ImageBinary}).

get_messages(ClientPid) ->
    gen_server:call(ClientPid, get_messages).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ClientName, RoomName]) ->
    io:format("Client ~s connecting to room ~s~n", [ClientName, RoomName]),

    % Join the room
    ok = chat_room:join(RoomName, self()),

    {ok, #state{name = ClientName, room_name = RoomName}}.

handle_call(get_name, _From, State = #state{name = Name}) ->
    {reply, Name, State};

handle_call(get_messages, _From, State = #state{messages = Messages}) ->
    {reply, lists:reverse(Messages), State#state{messages = []}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({send_message, Message}, State = #state{name = Name, room_name = RoomName}) ->
    chat_room:send_message(RoomName, Name, Message),
    {noreply, State};

handle_cast({send_image, ImageBinary}, State = #state{name = Name, room_name = RoomName}) ->
    io:format("~s sending image (~p bytes) to room ~s~n",
              [Name, byte_size(ImageBinary), RoomName]),
    chat_room:send_image(RoomName, Name, ImageBinary),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

% Receive messages broadcast from room
handle_info({message, FromName, Message}, State = #state{name = MyName, messages = Messages}) ->
    io:format("[~s] ~s: ~s~n", [MyName, FromName, Message]),
    NewMessages = [{text, FromName, Message} | Messages],
    {noreply, State#state{messages = NewMessages}};

handle_info({image, FromName, ImageBinary}, State = #state{name = MyName, messages = Messages}) ->
    ImageSize = byte_size(ImageBinary),
    io:format("[~s] ~s sent an image (~p bytes)~n", [MyName, FromName, ImageSize]),

    % Client DOES process the image and then discards the reference
    % This is good - clients do work, trigger GC, and clean up
    NewMessages = [{image, FromName, ImageSize} | Messages],
    {noreply, State#state{messages = NewMessages}};

handle_info({user_joined, Name}, State = #state{name = MyName}) ->
    io:format("[~s] *** ~s joined the room~n", [MyName, Name]),
    {noreply, State};

handle_info({user_left, Name}, State = #state{name = MyName}) ->
    io:format("[~s] *** ~s left the room~n", [MyName, Name]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{name = Name, room_name = RoomName}) ->
    io:format("Client ~s disconnecting from room ~s~n", [Name, RoomName]),
    chat_room:leave(RoomName, self()),
    ok.
