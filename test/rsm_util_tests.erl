%%% @copyright 2016+ Hippware, Inc.
%%% @doc Test suite for wocky_db_bot.erl
-module(rsm_util_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include("wocky.hrl").

-import(rsm_util, [get_rsm/1, filter_with_rsm/2]).

-define(COUNT, 200).

rsm_util_test_() -> {
  "rsm_util:filter_with_rsm",
  [ {inparallel, [
      test_get_rsm(),
      test_get_all(),
      test_get_limit(),
      test_get_by_id(),
      test_get_by_index(),
      test_before(),
      test_reverse()
    ]}
  ]}.

test_get_rsm() ->
    { "decodes wocky_rsm_in record from IQ", [
      { "decodes standard RSM", [
        ?_assertEqual({ok, rsm_in(0, undefined, undefined, undefined)},
                      get_rsm(
                        iq_with_rsm(0, undefined, undefined,
                                    undefined, false))),
        ?_assertEqual({ok, rsm_in(5, aft, <<"abc">>, undefined, true)},
                      get_rsm(
                        iq_with_rsm(5, aft, <<"abc">>,
                                    undefined, true)))
      ]},
      { "returns an error on missing RSM", [
        ?_assertMatch({error, _},
                      get_rsm(#iq{id = <<"abc">>, type = <<"result">>,
                                  sub_el = #xmlel{name = <<"blah">>}}))
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

test_reverse() ->
    [
     { "reverse tag should reverse results and leave everything else", [
       ?_assert(is_result_reversed(
                  filter_with_rsm(data(),
                                  rsm_in(10, before,
                                         undefined, undefined)),
                  filter_with_rsm(data(),
                                  rsm_in(10, before,
                                         undefined, undefined, true)))),
       ?_assert(is_result_reversed(
                  filter_with_rsm(data(), rsm_in(10, aft,
                                                 undefined, 50)),
                  filter_with_rsm(data(), rsm_in(10, aft,
                                                 undefined, 50, true))))
     ]}
    ].


rsm_in(Max, Dir, ID, Index) ->
    rsm_in(Max, Dir, ID, Index, false).
rsm_in(Max, Dir, ID, Index, Reverse) ->
    #wocky_rsm_in{max = Max, direction = Dir, id = ID,
                  index = Index, reverse = Reverse}.

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

is_result_reversed({L1, R1}, {L2, R2}) ->
    L1 =:= lists:reverse(L2)
    andalso
    R1 =:= R2.

iq_with_rsm(Max, Dir, ID, Index, Reverse) ->
    #iq{id = <<"some id">>, type = <<"request">>,
        sub_el = #xmlel{name = <<"operation">>,
                        children = maybe_rand_tags() ++
                        [rsm_tag(Max, Dir, ID, Index, Reverse)] ++
                        maybe_rand_tags()}}.

maybe_rand_tags() ->
    %% Produce 0..2 random tags
    [rand_tag() || _ <- lists:duplicate(rand:uniform(3)-1, a)].

rand_tag() ->
    X = integer_to_binary(rand:uniform(1000)),
    #xmlel{name = X, children = [#xmlcdata{content = X}]}.

rsm_tag(Max, Dir, ID, Index, Reverse) ->
    #xmlel{name = <<"set">>,
           attrs = [{<<"xmlns">>, ?NS_RSM}],
           children = rsm_children(Max, Dir, ID, Index, Reverse, [])}.

rsm_children(undefined, Dir, ID, Index, Reverse, Acc) ->
    rsm_children(Dir, ID, Index, Reverse, Acc);
rsm_children(Max, Dir, ID, Index, Reverse, Acc) ->
    rsm_children(Dir, ID, Index, Reverse,
                 [xml_with_cdata(<<"max">>, integer_to_binary(Max)) | Acc]).

rsm_children(undefined, _ID, Index, Reverse, Acc) ->
    rsm_children(Index, Reverse, Acc);
rsm_children(before, ID, Index, Reverse, Acc) ->
    rsm_children(Index, Reverse, [dir_element(<<"before">>, ID) | Acc]);
rsm_children(aft, ID, Index, Reverse, Acc) ->
    rsm_children(Index, Reverse, [dir_element(<<"after">>, ID) | Acc]).

rsm_children(undefined, Reverse, Acc) ->
    rsm_children(Reverse, Acc);
rsm_children(Index, Reverse, Acc) ->
    rsm_children(Reverse,
                 [xml_with_cdata(<<"index">>, integer_to_binary(Index)) | Acc]).

rsm_children(false, Acc) -> Acc;
rsm_children(true, Acc) -> [#xmlel{name = <<"reverse">>} | Acc].

dir_element(Name, undefined) ->
    #xmlel{name = Name};
dir_element(Name, ID) ->
    xml_with_cdata(Name, ID).

xml_with_cdata(Name, CData) ->
    #xmlel{name = Name, children = [#xmlcdata{content = CData}]}.
