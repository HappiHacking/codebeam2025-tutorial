-module(chat_server_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([start_room/1, start_client/2]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 10},

    % Start room supervisor and client supervisor
    ChildSpecs = [
        #{id => room_sup,
          start => {chat_room_sup, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => supervisor,
          modules => [chat_room_sup]},

        #{id => client_sup,
          start => {chat_client_sup, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => supervisor,
          modules => [chat_client_sup]}
    ],

    {ok, {SupFlags, ChildSpecs}}.

%% API for starting rooms and clients dynamically
start_room(RoomName) ->
    chat_room_sup:start_room(RoomName).

start_client(ClientName, RoomName) ->
    chat_client_sup:start_client(ClientName, RoomName).
