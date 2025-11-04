-module(chat_client_sup).
-behaviour(supervisor).

-export([start_link/0, init/1, start_client/2]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 5,
                 period => 30},

    ChildSpec = #{id => chat_client,
                  start => {chat_client, start_link, []},
                  restart => transient,
                  shutdown => 5000,
                  type => worker,
                  modules => [chat_client]},

    {ok, {SupFlags, [ChildSpec]}}.

start_client(ClientName, RoomName) ->
    supervisor:start_child(?MODULE, [ClientName, RoomName]).
