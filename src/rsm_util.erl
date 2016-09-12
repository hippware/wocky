%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Utility functions for filtering a list with RSM
%%% See https://xmpp.org/extensions/xep-0059.html
%%%
-module(rsm_util).

-include_lib("ejabberd/include/jlib.hrl").

-define(EX_TO_UNDEFINED(F), try F catch _:_ -> undefined end).

-export([filter_with_rsm/2]).

%% This function takes a list of records and an RSM 
-spec filter_with_rsm([map()], jlib:rsm_in()) -> {[map()], jlib:rsm_out()}.
filter_with_rsm(Items, #rsm_in{id = undefined, index = undefined,
                               direction = before, max = undefined}) ->
    get_result_list(Items, Items, 0);

filter_with_rsm(Items, #rsm_in{id = undefined, index = undefined,
                               direction = before, max = C}) ->
    {Before, Result} = safesplit(length(Items) - C, Items),
    get_result_list(Items, Result, length(Before));

filter_with_rsm(Items, #rsm_in{id = undefined, index = undefined,
                               max = C}) ->
    {Result, _After} = safesplit(C, Items),
    get_result_list(Items, Result, 0);

filter_with_rsm(Items, #rsm_in{id = RSMID, max = C,
                               direction = before})
  when RSMID =/= undefined ->
    BeforeResult =
    lists:takewhile(fun(#{id := ID}) -> ID =/= RSMID end, Items),
    {Before, Result} = safesplit(length(BeforeResult) - C, BeforeResult),
    get_result_list(Items, Result, length(Before));

filter_with_rsm(Items, #rsm_in{id = RSMID, max = C})
  when RSMID =/= undefined ->
    {Before, ResultAfter} =
    split_include(fun(#{id := ID}) -> ID =/= RSMID end, Items),
    {Result, _After} = safesplit(C, ResultAfter),
    get_result_list(Items, Result, length(Before));

filter_with_rsm(Items, #rsm_in{index = Index, max = C}) ->
    {Before, ResultAfter} = safesplit(Index, Items),
    {Result, _After} = safesplit(C, ResultAfter),
    get_result_list(Items, Result, length(Before)).

get_result_list(Items, Result, FirstIndex) ->
    First = ?EX_TO_UNDEFINED(
               integer_to_binary(maps:get(id, hd(Result)))),
    Index = case First of
                undefined -> undefined;
                _ -> FirstIndex
            end,
    Last = ?EX_TO_UNDEFINED(
              integer_to_binary(maps:get(id, lists:last(Result)))),
    {Result, #rsm_out{count = length(Items), index = Index,
                      first = First, last = Last}}.

safesplit(N, List) when N < 0 ->
    {[], List};
safesplit(N, List) when N >= length(List) ->
    {List, []};
safesplit(N, List) ->
    lists:split(N, List).

%% Behaves as per lists:splitwith/2, but includes the first *NON*-satisfying
%% element in the first returned list rather than the second
-spec split_include(fun((A) -> boolean()), [A]) -> {[A], [A]}.
split_include(Pred, List) ->
    split_include(Pred, List, []).
split_include(_Pred, [], Acc) ->
    {Acc, []};
split_include(Pred, [H|T], Acc) ->
    case Pred(H) of
        true -> split_include(Pred, T, [H|Acc]);
        false -> {lists:reverse([H|Acc]), T}
    end.
