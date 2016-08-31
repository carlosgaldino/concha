-module(concha).

-define(HASH, sha256).

-type ring() :: {integer(), gb_trees:tree()}.

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
    Iter = gb_trees:iterator_from(HKey, Ring),
    case gb_trees:next(Iter) of
        {_, Node, _} -> Node;
        none -> element(2, gb_trees:smallest(Ring))
    end.

-spec add(term(), ring()) -> ring().
add(Node, {VNodesSize, Ring}) ->
    NewRing = build_ring(position_node(VNodesSize, Node), Ring),
    {VNodesSize, NewRing}.

-spec remove(term(), ring()) -> ring().
remove(Node, {VNodesSize, Ring}) ->
    Positions = position_node(VNodesSize, Node),
    NewRing = lists:foldl(fun({Pos, _}, Tree) -> gb_trees:delete_any(Pos, Tree) end, Ring, Positions),
    {VNodesSize, NewRing}.

%%====================================================================
%% Internal functions
%%====================================================================
build_ring(Nodes) ->
    gb_trees:from_orddict(lists:keysort(1, lists:flatten(Nodes))).

build_ring(Nodes, Ring) ->
    lists:foldl(fun({Pos, Node}, Tree) -> gb_trees:insert(Pos, Node, Tree) end, Ring, Nodes).

chash(X) -> crypto:hash(?HASH, term_to_binary(X)).

chash(X, Y) ->
    XBin = term_to_binary(X),
    YBin = term_to_binary(Y),
    crypto:hash(?HASH, <<XBin/binary, YBin/binary>>).

position_node(VNodesSize, Node) ->
    [{chash(Node, Idx), Node} || Idx <- lists:seq(1, VNodesSize)].
