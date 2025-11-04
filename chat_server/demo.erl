-module(demo).
-export([run/0, demonstrate_leak/0, stress_test/1]).

%% Main demo - shows the binary leak
run() ->
    io:format("=== Chat Server Demo: Binary Leak Problem ===~n~n"),

    % Start the application
    application:start(chat_server),

    % Create a room
    RoomName = <<"general">>,
    {ok, _RoomPid} = chat_server_sup:start_room(RoomName),
    io:format("Created room: ~s~n", [RoomName]),

    % Create some clients
    {ok, Alice} = chat_server_sup:start_client(<<"Alice">>, RoomName),
    {ok, Bob} = chat_server_sup:start_client(<<"Bob">>, RoomName),
    {ok, Carol} = chat_server_sup:start_client(<<"Carol">>, RoomName),

    timer:sleep(500),

    io:format("~n--- Sending text messages (normal behavior) ---~n"),

    % Send some text messages
    chat_client:send_message(Alice, <<"Hello everyone!">>),
    timer:sleep(100),
    chat_client:send_message(Bob, <<"Hi Alice!">>),
    timer:sleep(100),

    % Check room stats
    Stats1 = chat_room:get_stats(RoomName),
    print_stats("After text messages", Stats1),

    io:format("~n--- Now sending images (THIS CAUSES THE LEAK!) ---~n"),

    % Create a simulated image (1MB)
    Image1MB = crypto:strong_rand_bytes(1024 * 1024),
    io:format("Created 1MB test image~n"),

    % Send the image from Alice
    chat_client:send_image(Alice, Image1MB),
    timer:sleep(100),

    % Check room stats - memory will be higher
    Stats2 = chat_room:get_stats(RoomName),
    print_stats("After 1 image", Stats2),

    io:format("~n--- Sending more images to demonstrate accumulation ---~n"),

    % Send more images
    lists:foreach(fun(N) ->
        io:format("Sending image ~p...~n", [N]),
        chat_client:send_image(Bob, crypto:strong_rand_bytes(1024 * 1024)),
        timer:sleep(50)
    end, lists:seq(2, 5)),

    timer:sleep(500),

    % Final stats show the leak
    Stats3 = chat_room:get_stats(RoomName),
    print_stats("After 5 images total", Stats3),

    io:format("~n=== Analysis ===~n"),
    io:format("The room process memory grew from ~p to ~p bytes~n",
              [maps:get(memory_bytes, Stats1), maps:get(memory_bytes, Stats3)]),
    io:format("This is because:~n"),
    io:format("1. Room receives large binary in message~n"),
    io:format("2. Room broadcasts to all members (creates more references)~n"),
    io:format("3. Room does minimal work, so GC rarely triggers~n"),
    io:format("4. ProcBin references stay alive in room's heap~n"),
    io:format("5. Even 'processed' messages keep references until GC runs~n~n"),

    io:format("Try: observer:start() and look at the chat_room process!~n~n"),

    ok.

%% Demonstrate the leak more dramatically
demonstrate_leak() ->
    io:format("=== Demonstrating Severe Binary Leak ===~n~n"),

    application:start(chat_server),

    RoomName = <<"leak_demo">>,
    {ok, _} = chat_server_sup:start_room(RoomName),

    {ok, Client1} = chat_server_sup:start_client(<<"Client1">>, RoomName),
    {ok, _Client2} = chat_server_sup:start_client(<<"Client2">>, RoomName),

    timer:sleep(200),

    io:format("Sending 20 images of 1MB each...~n"),

    lists:foreach(fun(N) ->
        Image = crypto:strong_rand_bytes(1024 * 1024),
        chat_client:send_image(Client1, Image),

        if N rem 5 =:= 0 ->
            Stats = chat_room:get_stats(RoomName),
            io:format("After ~p images: ~p MB room memory~n",
                      [N, maps:get(memory_bytes, Stats) div (1024*1024)]);
        true ->
            ok
        end,
        timer:sleep(100)
    end, lists:seq(1, 20)),

    FinalStats = chat_room:get_stats(RoomName),
    io:format("~nFinal room memory: ~p MB~n",
              [maps:get(memory_bytes, FinalStats) div (1024*1024)]),

    io:format("~nThe room process is now holding references to all 20 images!~n"),
    io:format("Solution: Force GC or use binary:copy/1 for small slices~n~n"),

    ok.

%% Stress test to really show the problem
stress_test(NumImages) ->
    io:format("=== Stress Test: ~p images ===~n", [NumImages]),

    application:start(chat_server),

    RoomName = <<"stress">>,
    {ok, _} = chat_server_sup:start_room(RoomName),
    {ok, Client} = chat_server_sup:start_client(<<"StressClient">>, RoomName),

    timer:sleep(200),

    StartStats = chat_room:get_stats(RoomName),
    io:format("Starting memory: ~p KB~n",
              [maps:get(memory_bytes, StartStats) div 1024]),

    lists:foreach(fun(N) ->
        Image = crypto:strong_rand_bytes(512 * 1024),  % 512KB each
        chat_client:send_image(Client, Image),
        timer:sleep(20)
    end, lists:seq(1, NumImages)),

    timer:sleep(500),

    EndStats = chat_room:get_stats(RoomName),
    io:format("Ending memory: ~p MB~n",
              [maps:get(memory_bytes, EndStats) div (1024*1024)]),

    MemoryGrowth = maps:get(memory_bytes, EndStats) - maps:get(memory_bytes, StartStats),
    io:format("Memory growth: ~p MB~n", [MemoryGrowth div (1024*1024)]),

    io:format("~nExpected if no leak: ~1 MB (just state)~n"),
    io:format("Actual: ~p MB (holding references to all images!)~n",
              [maps:get(memory_bytes, EndStats) div (1024*1024)]),

    ok.

%% Helper
print_stats(Label, Stats) ->
    io:format("~n~s:~n", [Label]),
    io:format("  Members: ~p~n", [maps:get(member_count, Stats)]),
    io:format("  Messages: ~p~n", [maps:get(message_count, Stats)]),
    io:format("  Memory: ~p bytes (~p KB)~n",
              [maps:get(memory_bytes, Stats),
               maps:get(memory_bytes, Stats) div 1024]),
    io:format("  Queue: ~p~n", [maps:get(queue_length, Stats)]).
