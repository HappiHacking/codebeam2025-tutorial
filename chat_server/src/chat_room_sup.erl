-module(chat_room_sup).
-behaviour(supervisor).

-export([start_link/0, init/1, start_room/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 5,
                 period => 30},

    ChildSpec = #{id => chat_room,
                  start => {chat_room, start_link, []},
                  restart => transient,
                  shutdown => 5000,
                  type => worker,
                  modules => [chat_room]},

    {ok, {SupFlags, [ChildSpec]}}.

start_room(RoomName) ->
    supervisor:start_child(?MODULE, [RoomName]).
