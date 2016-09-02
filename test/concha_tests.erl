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
    %% the distribution for the following nodes will be: serverC, serverD, serverB, serverA.
    Nodes = ["serverA", "serverB", "serverC", "serverD"],
    Ring = concha:new(Nodes),
    ?assertEqual("serverB", concha:lookup("Mars", Ring)),
    ?assertEqual("serverB", concha:lookup("Jupiter", Ring)),
    %% "Europa" maps to the interval between the last node (serverA in this case) and the first (serverC).
    ?assertEqual("serverC", concha:lookup("Europa", Ring)),
    %% keys should fall in the partition where node id is >= key id
    ?assertEqual("serverA", concha:lookup("serverA", Ring)).

lookup_with_vnodes_test() ->
    Nodes = ["serverA", "serverB", "serverC", "serverD"],
    Ring = concha:new(3, Nodes),
    ?assertEqual("serverB", concha:lookup("Mars", Ring)),
    ?assertEqual("serverA", concha:lookup("Venus", Ring)),
    ?assertEqual("serverB", concha:lookup("Saturn", Ring)),
    ?assertEqual("serverD", concha:lookup("Europa", Ring)),
    ?assertEqual("serverB", concha:lookup("Jupiter", Ring)),
    %% keys should fall in the partition where node id is >= key id
    ?assertEqual("serverA", concha:lookup("serverA", Ring)).

lookup_empty_ring_test() ->
    Ring = concha:new([]),
    ?assertEqual({error, empty_ring}, concha:lookup("Mars", Ring)),
    Ring2 = concha:new(3, []),
    ?assertEqual({error, empty_ring}, concha:lookup("Mars", Ring2)).

add_test() ->
    Nodes = ["serverA", "serverB", "serverC", "serverD"],
    %% serverE will lie between serverD and serverB
    Ring = concha:add("serverE", concha:new(Nodes)),
    ?assertEqual("serverB", concha:lookup("Mars", Ring)),
    ?assertEqual("serverA", concha:lookup("Venus", Ring)),
    ?assertEqual("serverE", concha:lookup("Jupiter", Ring)),
    ?assertEqual("serverC", concha:lookup("Europa", Ring)).

add_with_vnodes_test() ->
    Nodes = ["serverA", "serverB", "serverC", "serverD"],
    Ring = concha:add("serverE", concha:new(3, Nodes)),
    ?assertEqual("serverB", concha:lookup("Mars", Ring)),
    ?assertEqual("serverA", concha:lookup("Venus", Ring)),
    ?assertEqual("serverE", concha:lookup("Jupiter", Ring)),
    ?assertEqual("serverD", concha:lookup("Europa", Ring)).

remove_test() ->
    Nodes = ["serverA", "serverB", "serverC", "serverD"],
    Ring = concha:remove("serverB", concha:new(Nodes)),
    ?assertEqual("serverA", concha:lookup("Mars", Ring)),
    ?assertEqual("serverA", concha:lookup("Venus", Ring)),
    ?assertEqual("serverA", concha:lookup("Jupiter", Ring)),
    ?assertEqual("serverC", concha:lookup("Europa", Ring)).

remove_with_vnodes_test() ->
    Nodes = ["serverA", "serverB", "serverC", "serverD"],
    Ring = concha:remove("serverB", concha:new(3, Nodes)),
    ?assertEqual("serverA", concha:lookup("Mars", Ring)),
    ?assertEqual("serverA", concha:lookup("Venus", Ring)),
    ?assertEqual("serverA", concha:lookup("Jupiter", Ring)),
    ?assertEqual("serverD", concha:lookup("Europa", Ring)),
    ?assertEqual("serverC", concha:lookup("Pluto", Ring)).

size_test() ->
    ?assertEqual(0, concha:size(concha:new([]))),
    Nodes = ["serverA", "serverB", "serverC", "serverD"],
    ?assertEqual(4, concha:size(concha:new(Nodes))),
    ?assertEqual(20, concha:size(concha:new(5, Nodes))).

members_test() ->
    ?assertEqual([], concha:members(concha:new([]))),
    Nodes = ["serverA", "serverB", "serverC", "serverD"],
    ?assertEqual(Nodes, concha:members(concha:new(Nodes))),
    ?assertEqual(Nodes, concha:members(concha:new(5, Nodes))).

contains_test() ->
    ?assertEqual(false, concha:contains("serverA", concha:new([]))),
    Nodes = ["serverA", "serverB", "serverC", "serverD"],
    ?assertEqual(true, concha:contains("serverD", concha:new(Nodes))),
    ?assertEqual(false, concha:contains("serverE", concha:new(Nodes))),
    ?assertEqual(true, concha:contains("serverD", concha:new(5, Nodes))),
    ?assertEqual(false, concha:contains("serverE", concha:new(5, Nodes))).
