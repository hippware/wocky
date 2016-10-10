%%% @copyright 2016+ Hippware, Inc.
%%% @doc Wocky utility functions
%%%
%%% This is a module for wocky utility functions that don't obviously warrant
%%% their own module but are used from multiple places.
-module(wocky_util).

-include_lib("ejabberd/include/jlib.hrl").
-include("wocky_roster.hrl").

-export([
   add_hooks/4,
   delete_hooks/4,
   safe_bin_to_integer/1,
   default_bin_to_integer/2,
   safe_bin_to_float/1,
   coord_to_binary/1,
   null_to_list/1,
   intersection/2,

   binary_as_hex/1,

   set_config_from_opt/4,

   archive_jid/1,

   iq_id/0,
   make_error_iq_response/2,

   is_friend/2
        ]).

-export_type([hook/0]).

-compile({parse_transform, cut}).

% Not used externally right now, but we want it available:
-ignore_xref([safe_bin_to_integer/1]).
% Currently only used by tests:
-ignore_xref([iq_id/0]).

-type hook() :: {Hook :: atom(), Callback :: atom()}.

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

-spec set_config_from_opt(atom(), atom(), term(), proplists:proplist()) -> ok.
set_config_from_opt(OptTag, Config, Default, Opts) ->
    Val = proplists:get_value(OptTag, Opts, Default),
    {atomic, _} = ejabberd_config:add_local_option(Config, Val),
    ok.

-spec archive_jid(jid()) -> binary().
archive_jid(JID) -> jid:to_binary(jid:to_bare(JID)).

-spec iq_id() -> binary().
iq_id() ->
    Int = erlang:unique_integer([positive]),
    integer_to_binary(Int, 36).

-spec make_error_iq_response(iq(), jlib:xmlel() | [jlib:xmlel()]) -> iq().
make_error_iq_response(IQ, ErrStanza) ->
    ok = lager:warning("Error on user IQ request: ~p", [ErrStanza]),
    IQ#iq{type = error, sub_el = ErrStanza}.

-spec coord_to_binary(float()) -> binary().
coord_to_binary(Coordinate) ->
    % 6 places after the decimal gives us a resolution at the equator of
    % roughly 11cm, while still staying well away from nasty floating point
    % rouding errors on 64 bit floats
    float_to_binary(Coordinate, [{decimals, 6}]).

-spec null_to_list(not_found | null | list()) -> list() | not_found.
null_to_list(null) -> [];
null_to_list(L) -> L.

-spec intersection(list(), list()) -> list().
intersection(A, B) ->
    lists:filter(lists:member(_, B), A).

-spec binary_as_hex(binary()) -> binary().
binary_as_hex(Input) ->
    << <<Y>> || <<X:4>> <= Input, Y <- integer_to_list(X, 16)>>.

-spec is_friend(subscription_type(), [binary()]) -> boolean().
is_friend(Subscription, Groups) ->
    Subscription =:= both
    andalso
    not lists:member(<<"__blocked__">>, Groups).
