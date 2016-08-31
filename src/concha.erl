-module(concha).

-define(HASH, sha256).

-type ring() :: {integer(), [{binary(), term()}]}.

%% API exports
-export([new/1,
         new/2,
         lookup/2,
         add/2,
         remove/2]).

%%====================================================================
%% API functions
%%====================================================================
-spec new([term()]) -> ring().
new(Nodes) ->
    new(1, Nodes).

-spec new(integer(), [term()]) -> ring().
new(VNodesSize, Nodes) ->
    Ring = build_ring([position_node(VNodesSize, Node) || Node <- Nodes]),
    {VNodesSize, Ring}.

-spec lookup(term(), ring()) -> term().
lookup(Key, {_VNodesSize, Ring}) ->
    HKey = chash(Key),
    find_node(HKey, Ring).

-spec add(term(), ring()) -> ring().
add(Node, {VNodesSize, Ring}) ->
    NewRing = build_ring([position_node(VNodesSize, Node) | Ring]),
    {VNodesSize, NewRing}.

-spec remove(term(), ring()) -> ring().
remove(Node, {VNodesSize, Ring}) ->
    {VNodesSize, build_ring(remove(Node, Ring, []))}.

%%====================================================================
%% Internal functions
%%====================================================================
build_ring(Nodes) ->
    lists:keysort(1, lists:flatten(Nodes)).

chash(X) -> crypto:hash(?HASH, term_to_binary(X)).

chash(X, Y) ->
    XBin = term_to_binary(X),
    YBin = term_to_binary(Y),
    crypto:hash(?HASH, <<XBin/binary, YBin/binary>>).

position_node(VNodesSize, Node) ->
    [{chash(Node, Idx), Node} || Idx <- lists:seq(1, VNodesSize)].

find_node(Key, Ring) ->
    find_node(Key, Ring, Ring).

find_node(_Key, [], Ring) ->
    {_Pos, Node} = hd(Ring),
    Node;
find_node(Key, [{Position, Node} | T], Ring) ->
    case Position >= Key of
        true -> Node;
        false -> find_node(Key, T, Ring)
    end.

remove(_Node, [], Acc) ->
    Acc;
remove(Node, [{_Pos, Node} | T], Acc) ->
    remove(Node, T, Acc);
remove(Node, [X | T], Acc) ->
    remove(Node, T, [X | Acc]).
