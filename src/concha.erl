-module(concha).

%% API exports
-export([add/2,
         lookup/2,
         members/1,
         new/1,
         new/2,
         remove/2,
         size/1]).

-export_type([ring/0]).

-define(HASH, sha256).

-type num_vnodes() :: pos_integer().
-type node_entry() :: term().
-type key() :: term().
-type position() :: binary().

-type positions() :: [{position(), node_entry()}].
-type nodes() :: [node_entry()].

-type inner_ring() :: gb_trees:tree(position(), node_entry()).

-opaque ring() :: {num_vnodes(), inner_ring()}.

%%====================================================================
%% API functions
%%====================================================================
-spec add(node_entry(), Ring::ring()) -> ring().
add(Node, {NumVNodes, InnerRing}) ->
    NewInnerRing = build_ring(position_node(NumVNodes, Node), InnerRing),
    {NumVNodes, NewInnerRing}.

-spec lookup(key(), Ring::ring()) -> node_entry() | {error, empty_ring}.
lookup(Key, {_NumVNodes, InnerRing}) ->
    case gb_trees:is_empty(InnerRing) of
        true -> {error, empty_ring};
        false ->
            HKey = chash(Key),
            Iter = gb_trees:iterator_from(HKey, InnerRing),
            case gb_trees:next(Iter) of
                {_, Node, _} -> Node;
                none -> element(2, gb_trees:smallest(InnerRing))
            end
    end.

-spec members(Ring::ring()) -> nodes().
members({_NumVNodes, InnerRing}) ->
    lists:usort(gb_trees:values(InnerRing)).

-spec new(nodes()) -> ring().
new(Nodes) ->
    new(1, Nodes).

-spec new(num_vnodes(), nodes()) -> ring().
new(NumVNodes, Nodes) ->
    Ring = build_ring(lists:flatten([position_node(NumVNodes, Node) || Node <- Nodes])),
    {NumVNodes, Ring}.

-spec remove(node_entry(), Ring::ring()) -> ring().
remove(Node, {NumVNodes, InnerRing}) ->
    Positions = position_node(NumVNodes, Node),
    NewInnerRing = lists:foldl(fun({Pos, _}, Tree) -> gb_trees:delete_any(Pos, Tree) end, InnerRing, Positions),
    {NumVNodes, NewInnerRing}.

-spec size(Ring::ring()) -> non_neg_integer().
size({_NumVNodes, InnerRing}) ->
    gb_trees:size(InnerRing).

%%====================================================================
%% Internal functions
%%====================================================================
-spec build_ring(positions()) -> inner_ring().
build_ring(Nodes) ->
    gb_trees:from_orddict(lists:keysort(1, Nodes)).

-spec build_ring(positions(), inner_ring()) -> inner_ring().
build_ring(Nodes, Ring) ->
    lists:foldl(fun({Pos, Node}, Tree) -> gb_trees:insert(Pos, Node, Tree) end, Ring, Nodes).

chash(X) -> crypto:hash(?HASH, term_to_binary(X)).

chash(X, Y) ->
    XBin = term_to_binary(X),
    YBin = term_to_binary(Y),
    crypto:hash(?HASH, <<XBin/binary, YBin/binary>>).

-spec position_node(num_vnodes(), node_entry()) -> positions().
position_node(NumVNodes, Node) ->
    [{chash(Node, Idx), Node} || Idx <- lists:seq(1, NumVNodes)].
