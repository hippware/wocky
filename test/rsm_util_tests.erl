%%% @copyright 2016+ Hippware, Inc.
%%% @doc Test suite for wocky_db_bot.erl
-module(rsm_util_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/jlib.hrl").

-import(rsm_util, [filter_with_rsm/2]).

-define(COUNT, 200).

rsm_util_test_() -> {
  "rsm_util:filter_with_rsm",
  [ {inparallel, [
      test_get_all(),
      test_get_limit(),
      test_get_by_id(),
      test_get_by_index(),
      test_before()
    ]}
  ]}.

test_get_all() ->
    { "returns records unchanged when no RSM values are set", [
      ?_assertEqual({data(), rsm_out(0, 1, ?COUNT)},
                     filter_with_rsm(data(),
                                     rsm_in(undefined, aft,
                                            undefined, undefined))),
      ?_assertEqual({unordered_data(),
                     rsm_out(0, unordered_id(1), unordered_id(?COUNT))},
                    filter_with_rsm(unordered_data(),
                                    rsm_in(undefined, aft,
                                           undefined, undefined))),
      ?_assertEqual({[], rsm_out(0, undefined, undefined, undefined)},
                    filter_with_rsm([], rsm_in(undefined, aft,
                                               undefined, undefined)))
    ]}.

test_get_limit() ->
    { "gets a restricted subset of the first X results", [
      ?_assertEqual({lists:sublist(data(), 50), rsm_out(0, 1, 50)},
                     filter_with_rsm(data(),
                                     rsm_in(50, aft,
                                            undefined, undefined))),
      ?_assertEqual({lists:sublist(unordered_data(), 50),
                     rsm_out(0, unordered_id(1), unordered_id(50))},
                    filter_with_rsm(unordered_data(),
                                    rsm_in(50, aft,
                                           undefined, undefined))),
      ?_assertEqual({[], rsm_out(0, undefined, undefined, undefined)},
                    filter_with_rsm([], rsm_in(50, aft,
                                               undefined, undefined)))
    ]}.


test_get_by_id() ->
    [
     { "gets items by a particular id", [
       ?_assertEqual({lists:sublist(data(), 51, 10), rsm_out(50, 51, 60)},
                      filter_with_rsm(data(),
                                      rsm_in(10, aft,
                                             50, undefined))),
       ?_assertEqual({lists:sublist(unordered_data(), 51, 10),
                      rsm_out(50, unordered_id(51), unordered_id(60))},
                     filter_with_rsm(unordered_data(),
                                     rsm_in(10, aft,
                                            unordered_id(50), undefined)))
 
     ]},
     { "gets an empty result set for an invalid ID", [
       ?_assertEqual({[], rsm_out(undefined, undefined, undefined)},
                     filter_with_rsm(data(),
                                     rsm_in(10, aft, ?COUNT+50, undefined))),
       ?_assertEqual({[], rsm_out(undefined, undefined, undefined)},
                     filter_with_rsm(data(),
                                     rsm_in(10, aft, -100, undefined))),
       ?_assertEqual({[], rsm_out(0, undefined, undefined, undefined)},
                     filter_with_rsm([],
                                     rsm_in(10, aft, -100, undefined)))
     ]}
    ].

test_get_by_index() ->
    [
     { "gets items by a particular index", [
       ?_assertEqual({lists:sublist(data(), 51, 10), rsm_out(50, 51, 60)},
                      filter_with_rsm(data(),
                                      rsm_in(10, aft,
                                             undefined, 50))),
       ?_assertEqual({lists:sublist(unordered_data(), 51, 10),
                      rsm_out(50, unordered_id(51), unordered_id(60))},
                     filter_with_rsm(unordered_data(),
                                     rsm_in(10, aft,
                                            undefined, 50)))
     ]},
     { "returns an empty set for an out of range index", [
       ?_assertEqual({[], rsm_out(undefined, undefined, undefined)},
                     filter_with_rsm(data(),
                                     rsm_in(10, aft, undefined, ?COUNT+50)))
     ]}
    ].

test_before() ->
    [
     { "gets items at the end of the list", [
       ?_assertEqual({element(2, lists:split(?COUNT-10, data())),
                      rsm_out(?COUNT-10, ?COUNT-9, ?COUNT)},
                     filter_with_rsm(data(),
                                     rsm_in(10, before, undefined, undefined)))
     ]},
     { "gets items before an ID", [
        ?_assertEqual({lists:sublist(
                         element(2, lists:split(?COUNT-60, data())), 10),
                       rsm_out(?COUNT-60, ?COUNT-59, ?COUNT-50)},
                      filter_with_rsm(data(),
                                      rsm_in(10, before, ?COUNT-49, undefined)))
     ]},
     { "gets an empty result set for an invalid 'before' ID", [
       ?_assertEqual({[], rsm_out(undefined, undefined, undefined)},
                     filter_with_rsm(data(),
                                     rsm_in(10, before, ?COUNT+50, undefined))),
       ?_assertEqual({[], rsm_out(undefined, undefined, undefined)},
                     filter_with_rsm(data(),
                                     rsm_in(10, before, -100, undefined)))
     ]}
    ].


rsm_in(Max, Dir, ID, Index) ->
    #rsm_in{max = Max, direction = Dir, id = ID, index = Index}.

rsm_out(Index, First, Last) ->
    rsm_out(?COUNT, Index, First, Last).
rsm_out(Count, Index, First, Last) ->
    #rsm_out{count = Count, index = Index,
             first = maybe_to_binary(First),
             last = maybe_to_binary(Last)}.

maybe_to_binary(undefined) -> undefined;
maybe_to_binary(Int) when is_integer(Int) ->
    integer_to_binary(Int).

data() ->
    [#{id => ID, val => erlang:phash2(ID)} || ID <- lists:seq(1, ?COUNT)].

unordered_data() ->
    [#{id => unordered_id(ID), val => ID} || ID <- lists:seq(1, ?COUNT)].

unordered_id(Index) -> erlang:phash2(Index).
