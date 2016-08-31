-module(concha).

-define(HASH, sha256).

-type ring() :: [{binary(), term()}].

%% API exports
-export([new/1,
         lookup/2,
         add/2,
         remove/2]).

%%====================================================================
%% API functions
%%====================================================================
-spec new([term()]) -> ring().
new(Nodes) ->
    Positions = lists:map(fun position_node/1, Nodes),
    lists:keysort(1, Positions).

-spec lookup(term(), ring()) -> term().
lookup(Key, Ring) ->
    HKey = chash(Key),
    find_node(HKey, Ring).

-spec add(term(), ring()) -> ring().
add(Node, Ring) ->
    lists:keysort(1, [position_node(Node) | Ring]).

-spec remove(term(), ring()) -> ring().
remove(Node, Ring) ->
    lists:keydelete(Node, 2, Ring).

%%====================================================================
%% Internal functions
%%====================================================================
chash(X) -> crypto:hash(?HASH, term_to_binary(X)).

-spec position_node(term()) -> {binary(), term()}.
position_node(Node) ->
    {chash(Node), Node}.

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
