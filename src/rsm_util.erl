%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Utility functions for filtering a list with RSM
%%% See https://xmpp.org/extensions/xep-0059.html
%%%
-module(rsm_util).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").
-include("wocky.hrl").

-compile({parse_transform, cut}).

-define(EX_TO_UNDEFINED(F), try F catch _:_ -> undefined end).
-define(RSM_MAX, 1000).

-export([get_rsm/1, rsm_decode/1, filter_with_rsm/2]).

-type wocky_rsm_in() :: #wocky_rsm_in{}.

-spec get_rsm(xmlel() | ejabberd:iq()) ->
    {error, xmlel()} | {ok, wocky_rsm_in()}.
get_rsm(IQ) ->
    case rsm_decode(IQ) of
        none -> {error, ?ERRT_BAD_REQUEST(
                           ?MYLANG, <<"Missing or invalid RSM values">>)};
        RSM = #wocky_rsm_in{} -> {ok, RSM}
    end.

%% rsm_decode and rsm_parse_element are copied from jlib.erl and
%% modified to support the `reverse` tag
-spec rsm_decode(xmlel() | ejabberd:iq()) ->
    none | wocky_rsm_in().
rsm_decode(#iq{sub_el=SubEl})->
    rsm_decode(SubEl);
rsm_decode(#xmlel{}=SubEl) ->
    case xml:get_subtag(SubEl,<<"set">>) of
        false ->
            none;
        #xmlel{name = <<"set">>, children = SubEls} ->
            lists:foldl(fun rsm_parse_element/2, #wocky_rsm_in{}, SubEls)
    end.

-spec rsm_parse_element(xmlel(), wocky_rsm_in()) -> wocky_rsm_in().
rsm_parse_element(#xmlel{name = <<"reverse">>, attrs = []}, RsmIn) ->
    RsmIn#wocky_rsm_in{reverse=true};
rsm_parse_element(#xmlel{name = <<"max">>, attrs = []}=Elem, RsmIn) ->
    CountStr = xml:get_tag_cdata(Elem),
    {Count, _} = string:to_integer(binary_to_list(CountStr)),
    RsmIn#wocky_rsm_in{max=Count};
rsm_parse_element(#xmlel{name = <<"before">>,
                         attrs = []}=Elem, RsmIn) ->
    UID = case xml:get_tag_cdata(Elem) of
              <<>> -> undefined;
              X -> X
          end,
    RsmIn#wocky_rsm_in{direction=before, id=UID};
rsm_parse_element(#xmlel{name = <<"after">>, attrs = []}=Elem, RsmIn) ->
    UID = xml:get_tag_cdata(Elem),
    RsmIn#wocky_rsm_in{direction=aft, id=UID};
rsm_parse_element(#xmlel{name = <<"index">>, attrs = []}=Elem, RsmIn) ->
    IndexStr = xml:get_tag_cdata(Elem),
    {Index, _} = string:to_integer(binary_to_list(IndexStr)),
    RsmIn#wocky_rsm_in{index=Index};
rsm_parse_element(_, RsmIn)->
    RsmIn.

%% This function takes a list of records and an RSM selection structure.
%% The records must be maps and must contain an `id' field which is to
%% be used for ID-based lookups.
-spec filter_with_rsm([map() | binary()], wocky_rsm_in()) ->
    {[map() | binary()], jlib:rsm_out()}.

filter_with_rsm(Items, RSM = #wocky_rsm_in{reverse = Reverse}) ->
    maybe_reverse(filter_with_rsm_impl(Items, RSM), Reverse).

filter_with_rsm_impl(Items, RSM = #wocky_rsm_in{max = undefined}) ->
    filter_with_rsm_impl(Items, RSM#wocky_rsm_in{max = max_results()});

filter_with_rsm_impl(Items, #wocky_rsm_in{id = undefined, index = undefined,
                                     direction = before, max = C}) ->
    {Before, Result} = safesplit(length(Items) - C, Items),
    get_result_list(Items, Result, length(Before));

filter_with_rsm_impl(Items, #wocky_rsm_in{id = undefined,
                                     index = undefined, max = C}) ->
    {Result, _After} = safesplit(C, Items),
    get_result_list(Items, Result, 0);

filter_with_rsm_impl(Items, #wocky_rsm_in{id = RSMID, max = C,
                                     direction = before})
  when RSMID =/= undefined ->
    {_AfterRev, BeforeResultRev} =
    split_include(id_not_equal(_, RSMID), lists:reverse(Items)),
    BeforeResult = lists:reverse(BeforeResultRev),
    {Before, Result} = safesplit(length(BeforeResult) - C, BeforeResult),
    get_result_list(Items, Result, length(Before));

filter_with_rsm_impl(Items, #wocky_rsm_in{id = RSMID, max = C})
  when RSMID =/= undefined ->
    {Before, ResultAfter} =
    split_include(id_not_equal(_, RSMID), Items),
    {Result, _After} = safesplit(C, ResultAfter),
    get_result_list(Items, Result, length(Before));

filter_with_rsm_impl(Items, #wocky_rsm_in{index = Index, max = C}) ->
    {Before, ResultAfter} = safesplit(Index, Items),
    {Result, _After} = safesplit(C, ResultAfter),
    get_result_list(Items, Result, length(Before)).

get_result_list(Items, Result, FirstIndex) ->
    First = ?EX_TO_UNDEFINED(get_id(hd(Result))),
    Index = case First of
                undefined -> undefined;
                _ -> FirstIndex
            end,
    Last = ?EX_TO_UNDEFINED(get_id(lists:last(Result))),
    {Result, #rsm_out{count = length(Items), index = Index,
                      first = First, last = Last}}.

get_id(#{id := ID}) -> get_id(ID);
get_id(ID) when is_integer(ID) -> integer_to_binary(ID);
get_id(ID) when is_binary(ID) -> (ID).

safesplit(N, List) when N < 0 ->
    {[], List};
safesplit(N, List) when N >= length(List) ->
    {List, []};
safesplit(N, List) ->
    lists:split(N, List).

maybe_reverse({Items, RSMOut}, true) ->
    {lists:reverse(Items), RSMOut};
maybe_reverse(R, false) ->
    R.

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

max_results() ->
    ejabberd_config:get_local_option_or_default(rsm_max, ?RSM_MAX).

id_not_equal(#{id := ID}, RSMID) ->
    id_not_equal(ID, RSMID);
id_not_equal(ID, RSMID) when is_binary(ID) orelse is_integer(ID) ->
    ID =/= RSMID.
