-module(concha).

-define(HASH, sha256).

-type ring() :: {integer(), gb_trees:tree()}.

%% API exports
-export([add/2,
         lookup/2,
         members/1,
         new/1,
         new/2,
         remove/2,
         size/1]).

%%====================================================================
%% API functions
%%====================================================================
-spec add(term(), ring()) -> ring().
add(Node, {VNodesSize, Ring}) ->
    NewRing = build_ring(position_node(VNodesSize, Node), Ring),
    {VNodesSize, NewRing}.

-spec lookup(term(), ring()) -> term() | {error, empty_ring}.
lookup(Key, {_VNodesSize, Ring}) ->
    case gb_trees:is_empty(Ring) of
        true -> {error, empty_ring};
        false ->
            HKey = chash(Key),
            Iter = gb_trees:iterator_from(HKey, Ring),
            case gb_trees:next(Iter) of
                {_, Node, _} -> Node;
                none -> element(2, gb_trees:smallest(Ring))
            end
    end.

-spec members(ring()) -> [term()].
members({_VNodesSize, Ring}) ->
    lists:usort(gb_trees:values(Ring)).

-spec new([term()]) -> ring().
new(Nodes) ->
    new(1, Nodes).

-spec new(integer(), [term()]) -> ring().
new(VNodesSize, Nodes) ->
    Ring = build_ring([position_node(VNodesSize, Node) || Node <- Nodes]),
    {VNodesSize, Ring}.

-spec remove(term(), ring()) -> ring().
remove(Node, {VNodesSize, Ring}) ->
    Positions = position_node(VNodesSize, Node),
    NewRing = lists:foldl(fun({Pos, _}, Tree) -> gb_trees:delete_any(Pos, Tree) end, Ring, Positions),
    {VNodesSize, NewRing}.

-spec size(ring()) -> integer().
size({_VNodesSize, Ring}) ->
    gb_trees:size(Ring).

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
