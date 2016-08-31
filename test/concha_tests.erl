-module(concha_tests).
-include_lib("eunit/include/eunit.hrl").

new_ring_test() ->
    Nodes = ["serverA", "serverB", "serverC", "serverD"],
    Ring1 = concha:new(Nodes),
    Ring2 = concha:new(Nodes),
    ?assertEqual(Ring1, Ring2).

new_ring_with_vnodes_test() ->
    Nodes = ["serverA", "serverB", "serverC", "serverD"],
    Ring1 = concha:new(4, Nodes),
    Ring2 = concha:new(4, Nodes),
    ?assertEqual(Ring1, Ring2).

lookup_test() ->
    %% the distribution for the following keys will be: serverC, serverB, serverD, serverA.
    Nodes = ["serverA", "serverB", "serverC", "serverD"],
    Ring = concha:new(Nodes),
    ?assertEqual("serverB", concha:lookup("mars", Ring)),
    ?assertEqual("serverD", concha:lookup("venus", Ring)),
    ?assertEqual("serverB", concha:lookup("saturn", Ring)),
    %% "pluto" maps to the interval between the last node (serverA in this case) and the first (serverC).
    ?assertEqual("serverC", concha:lookup("pluto", Ring)).

lookup_with_vnodes_test() ->
    Nodes = ["serverA", "serverB", "serverC", "serverD"],
    Ring = concha:new(3, Nodes),
    ?assertEqual("serverB", concha:lookup("mars", Ring)),
    ?assertEqual("serverA", concha:lookup("venus", Ring)),
    ?assertEqual("serverB", concha:lookup("saturn", Ring)),
    ?assertEqual("serverD", concha:lookup("europa", Ring)),
    ?assertEqual("serverC", concha:lookup("pluto", Ring)).

add_test() ->
    Nodes = ["serverA", "serverB", "serverC", "serverD"],
    %% serverE will lie between serverC and serverB
    Ring = concha:add("serverE", concha:new(Nodes)),
    ?assertEqual("serverE", concha:lookup("mars", Ring)),
    ?assertEqual("serverD", concha:lookup("venus", Ring)),
    ?assertEqual("serverE", concha:lookup("saturn", Ring)),
    ?assertEqual("serverC", concha:lookup("pluto", Ring)).

add_with_vnodes_test() ->
    Nodes = ["serverA", "serverB", "serverC", "serverD"],
    Ring = concha:add("serverE", concha:new(3, Nodes)),
    ?assertEqual("serverB", concha:lookup("mars", Ring)),
    ?assertEqual("serverA", concha:lookup("venus", Ring)),
    ?assertEqual("serverB", concha:lookup("saturn", Ring)),
    ?assertEqual("serverD", concha:lookup("europa", Ring)),
    ?assertEqual("serverC", concha:lookup("pluto", Ring)).

remove_test() ->
    Nodes = ["serverA", "serverB", "serverC", "serverD"],
    Ring = concha:remove("serverB", concha:new(Nodes)),
    ?assertEqual("serverD", concha:lookup("mars", Ring)),
    ?assertEqual("serverD", concha:lookup("venus", Ring)),
    ?assertEqual("serverD", concha:lookup("saturn", Ring)),
    ?assertEqual("serverD", concha:lookup("europa", Ring)),
    ?assertEqual("serverC", concha:lookup("pluto", Ring)).

remove_with_vnodes_test() ->
    Nodes = ["serverA", "serverB", "serverC", "serverD"],
    Ring = concha:remove("serverB", concha:new(3, Nodes)),
    ?assertEqual("serverC", concha:lookup("mars", Ring)),
    ?assertEqual("serverA", concha:lookup("venus", Ring)),
    ?assertEqual("serverC", concha:lookup("saturn", Ring)),
    ?assertEqual("serverD", concha:lookup("europa", Ring)),
    ?assertEqual("serverC", concha:lookup("pluto", Ring)).