%%% @copyright 2016+ Hippware, Inc.
%%% @doc Wocky utility functions
%%%
%%% This is a module for wocky utility functions that don't obviously warrant
%%% their own module but are used from multiple places.
-module(wocky_util).

-compile({parse_transform, fun_chain}).
-compile({parse_transform, cut}).

-include("wocky.hrl").
-include("wocky_roster.hrl").

-define(string, 'Elixir.String').

-export([
   add_hooks/4,
   delete_hooks/4,
   safe_bin_to_integer/1,
   default_bin_to_integer/2,
   safe_bin_to_float/1,
   safe_bin_to_boolean/1,
   coord_to_binary/1,
   nil_to_bin/1,
   intersection/2,
   intersection/3,

   set_config_from_opt/4,

   iq_id/0,
   make_error_iq_response/2,

   v1_uuid_order/2,

   remove_redundant_jids/1,

   remove_whitespace/1,

   check_foldl/3
        ]).

-type hook() :: {Hook :: atom(), Callback :: atom()}.
-export_type([hook/0]).


%% @doc Register a set of hooks with ejabberd's hook system
-spec add_hooks(
        Hooks :: [hook()],
        Host  :: binary(),
        Module :: atom(),
        Sequence :: non_neg_integer()) -> ok.
add_hooks(Hooks, Host, Module, Sequence) ->
    lists:foreach(
      fun ({Hook, Callback}) ->
              ejabberd_hooks:add(Hook, Host, Module, Callback, Sequence)
      end, Hooks).

%% @doc Deregister a set of hooks with ejabberd's hook system
-spec delete_hooks(
        Hooks :: [hook()],
        Host  :: binary(),
        Module :: atom(),
        Sequence :: non_neg_integer()) -> ok.
delete_hooks(Hooks, Host, Module, Sequence) ->
    lists:foreach(
      fun ({Hook, Callback}) ->
              ejabberd_hooks:delete(Hook, Host, Module, Callback, Sequence)
      end, Hooks).

-spec safe_bin_to_float(binary()) -> {ok, float()} | {error, bad_float}.
safe_bin_to_float(Bin) ->
    try
        Float = binary_to_float(Bin),
        {ok, Float}
    catch _:_ ->
        try
            Float2 = float(binary_to_integer(Bin)),
            {ok, Float2}
        catch _:_ ->
            {error, bad_float}
        end
    end.

-spec safe_bin_to_integer(binary()) -> {ok, integer()} | {error, bad_integer}.
safe_bin_to_integer(Bin) ->
    try
        Int = binary_to_integer(Bin),
        {ok, Int}
    catch _:_ ->
        {error, bad_integer}
    end.

-spec default_bin_to_integer(binary(), integer()) -> integer().
default_bin_to_integer(Bin, Default) ->
    case safe_bin_to_integer(Bin) of
        {ok, Int} -> Int;
        {error, bad_integer} -> Default
    end.

-spec safe_bin_to_boolean(binary()) -> {ok, boolean()} | {error, bad_boolean}.
safe_bin_to_boolean(Bin) ->
    try binary_to_existing_atom(Bin, utf8) of
        X when X =:= true orelse X =:= false -> {ok, X};
        _ -> {error, bad_boolean}
    catch
        _:_ -> {error, bad_boolean}
    end.

-spec set_config_from_opt(atom(), atom(), term(), proplists:proplist()) -> ok.
set_config_from_opt(OptTag, Config, Default, Opts) ->
    Val = proplists:get_value(OptTag, Opts, Default),
    {atomic, _} = ejabberd_config:add_local_option(Config, Val),
    ok.

-spec iq_id() -> binary().
iq_id() ->
    Int = erlang:unique_integer([positive]),
    integer_to_binary(Int, 36).

-spec make_error_iq_response(iq(), jlib:xmlel() | [jlib:xmlel()]) -> iq().
make_error_iq_response(IQ, ErrStanza) ->
    ok = lager:debug("Error on user IQ request: ~p", [ErrStanza]),
    IQ#iq{type = error, sub_el = ErrStanza}.

-spec coord_to_binary(float()) -> binary().
coord_to_binary(Coordinate) ->
    % 6 places after the decimal gives us a resolution at the equator of
    % roughly 11cm, while still staying well away from nasty floating point
    % rouding errors on 64 bit floats
    float_to_binary(Coordinate, [{decimals, 6}]).

-spec nil_to_bin(nil | binary()) -> binary().
nil_to_bin(nil) -> <<>>;
nil_to_bin(B) -> B.

-spec intersection(list(), list()) -> list().
intersection(A, B) ->
    intersection(A, B, fun erlang:'=:='/2).

%% Returns all elements EA from A where there is an element EB in B for which
%% EqualityFun(EA, EB) returns true.
-spec intersection(list(T), list(T), fun((T, T) -> boolean())) -> list(T).
intersection(A, B, EqualityFun) ->
    lists:filter(fun(E) -> lists:any(EqualityFun(E, _), B) end, A).

%% Sorting function to sort v1 UUIDs by time (as is done by C*)
-spec v1_uuid_order(binary(), binary()) -> boolean().
v1_uuid_order(UUID1, UUID2) ->
    uuid:get_v1_time(uuid:string_to_uuid(UUID1))
    =<
    uuid:get_v1_time(uuid:string_to_uuid(UUID2)).

% Remove non-bare jids where there is also a bare version in the list
-spec remove_redundant_jids([jid()]) -> [jid()].
remove_redundant_jids(JIDs) ->
    lists:filter(fun(JID) -> not redundant(JID, JIDs) end, JIDs).

redundant(#jid{lresource = <<>>}, _) -> false;
redundant(JID, JIDs) ->
    fun_chain:first(
      JID,
      jid:to_bare(),
      jid:are_equal(_),
      lists:any(JIDs)
     ).

% Strips all (unicode-defined) whitespace out of a binary string
-spec remove_whitespace(binary()) -> binary().
remove_whitespace(String) ->
    fun_chain:first(
      String,
      ?string:split(),
      list_to_binary()
     ).

% Foldl with error checking for each iteration
-spec check_foldl(fun((A, term()) -> {ok, term()} | {error, term()}),
                  term(), [A]) -> {ok, term()} | {error, term()}.
check_foldl(_, Acc, []) -> {ok, Acc};
check_foldl(Fun, Acc, [H | T]) ->
    case Fun(H, Acc) of
        {error, E} -> {error, E};
        {ok, Acc2} -> check_foldl(Fun, Acc2, T)
    end.
