%%% @copyright 2016+ Hippware, Inc.
%%% @doc Wocky backend for mod_privacy
-module(mod_privacy_wocky).

-behaviour(mod_privacy).

-export([init/2,
         get_default_list/2,
         get_list_names/2,
         get_privacy_list/3,
         forget_default_list/2,
         set_default_list/3,
         remove_privacy_list/3,
         replace_privacy_list/4,
         remove_user/2]).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/mod_privacy.hrl").

%%====================================================================
%% mod_privacy callbacks
%%====================================================================

init(_Host, _Opts) ->
    ok.

get_default_list(LUser, LServer) ->
    case wocky_db:select_one(LServer, privacy, default, #{user => LUser}) of
        not_found ->
            {error, not_found};
        null ->
            {error, not_found};
        DefaultList ->
            get_list(LUser, LServer, DefaultList)
    end.

get_list_names(LUser, LServer) ->
    case wocky_db:select_row(LServer, privacy, [default, lists],
                             #{user => LUser}) of
        not_found ->
            {error, not_found};
        #{default := Default, lists := Lists} ->
            {ok, {null_to_binary(Default), null_to_list(Lists)}}
    end.

get_privacy_list(LUser, LServer, Name) ->
    case get_list(LUser, LServer, Name) of
        {error, not_found} ->
            {error, not_found};
        {ok, {Name, Items}} ->
            {ok, Items}
    end.

forget_default_list(LUser, LServer) ->
    ok = wocky_db:update(LServer, privacy, #{default => null},
                         #{user => LUser}).

set_default_list(LUser, LServer, Name) ->
    case get_user_lists(LUser, LServer) of
        not_found ->
            {error, not_found};
        Lists ->
            maybe_set_default_list(LUser, LServer, Name, Lists)
    end.

remove_privacy_list(LUser, LServer, Name) ->
    case wocky_db:select_row(LServer, privacy, [default, lists],
                             #{user => LUser}) of
        not_found ->
            ok;
        #{default := Name} ->
            {error, conflict};
        #{lists := Lists} ->
            maybe_delete_list(LUser, LServer, Name, null_to_list(Lists)),
            ok
    end.

replace_privacy_list(LUser, LServer, Name, Items) ->
    case get_user_lists(LUser, LServer) of
        not_found ->
            ok;
        Lists ->
            maybe_delete_list(LUser, LServer, Name, Lists)
    end,
    add_list(LUser, LServer, Name, Items),
    ok.

remove_user(LUser, LServer) ->
    case get_user_lists(LUser, LServer) of
        not_found ->
            ok;
        Lists ->
            lists:foreach(fun(L) -> delete_list(LUser, LServer, L) end,
                          Lists),
            ok = wocky_db:delete(LServer, privacy, all, #{user => LUser})
    end.

%%====================================================================
%% Helpers
%%====================================================================

get_user_lists(LUser, LServer) ->
    null_to_list(
      wocky_db:select_one(LServer, privacy, lists, #{user => LUser})).

get_list(LUser, LServer, Name) ->
    case wocky_db:select_one(LServer, privacy_list, items,
                             #{user => LUser, name => Name}) of
        not_found ->
            {error, not_found};
        ItemIDs ->
            {ok, {Name, get_list_items(LServer, ItemIDs)}}
    end.

get_list_items(LServer, ItemIDs) ->
    [row_to_item(
       wocky_db:select_row(LServer, privacy_item, all, #{id => ID})) ||
     ID <- ItemIDs].

maybe_set_default_list(LUser, LServer, Name, Lists) ->
    case lists:member(Name, Lists) of
        false ->
            {error, not_found};
        true ->
            ok = wocky_db:update(LServer, privacy, #{default => Name},
                                 #{user => LUser})
    end.

maybe_delete_list(LUser, LServer, Name, Lists) ->
    case lists:member(Name, Lists) of
        false ->
            ok;
        true ->
            delete_list(LUser, LServer, Name)
    end.

delete_list(LUser, LServer, Name) ->
    ItemIDs = wocky_db:select_one(LServer, privacy_list, items,
                                  #{user => LUser, name => Name}),
    lists:foreach(
      fun(I) ->
              wocky_db:delete(LServer, privacy_item, all, #{id => I})
      end,
      ItemIDs),
    wocky_db:delete(LServer, privacy_list, all, #{user => LUser, name => Name}),
    delete_list_from_user(LUser, LServer, Name).

add_list(LUser, LServer, Name, Items) ->
    Rows = [item_to_row(I) || I <- Items],
    lists:foreach(fun(R) -> wocky_db:insert(LServer, privacy_item, R) end,
                  Rows),

    ItemIDs = [ID || #{id := ID} <- Rows],
    ok = wocky_db:insert(LServer, privacy_list,
                         #{user => LUser, name => Name, items => ItemIDs}),
    add_list_to_user(LUser, LServer, Name).

add_list_to_user(LUser, LServer, Name) ->
    modify_user_lists(LUser, LServer, Name, "+").
delete_list_from_user(LUser, LServer, Name) ->
    modify_user_lists(LUser, LServer, Name, "-").

modify_user_lists(LUser, LServer, Name, Op) ->
    Q = ["UPDATE privacy SET lists = lists ", Op, " ? WHERE user = ?"],
    V = #{user => LUser,
          lists => [Name]},
    {ok, void} = wocky_db:query(LServer, Q, V, quorum),
    ok.

row_to_item(#{
  type := Type,
  value := Value,
  action := Action,
  item_order := Order,
  match_all := MatchAll,
  match_iq := MatchIQ,
  match_message := MatchMessage,
  match_presence_in := MatchPresenceIn,
  match_presence_out := MatchPresenceOut
 }) ->
    #listitem{
       type = binary_to_atom(Type, utf8),
       value = binary_to_value(Type, Value),
       action = bool_to_action(Action),
       order = Order,
       match_all = MatchAll,
       match_iq = MatchIQ,
       match_message = MatchMessage,
       match_presence_in = MatchPresenceIn,
       match_presence_out = MatchPresenceOut
    }.

item_to_row(
    #listitem{
       type = Type,
       value = Value,
       action = Action,
       order = Order,
       match_all = MatchAll,
       match_iq = MatchIQ,
       match_message = MatchMessage,
       match_presence_in = MatchPresenceIn,
       match_presence_out = MatchPresenceOut
    }) ->
    #{
       id => ossp_uuid:make(v1, text),
       type => atom_to_binary(Type, utf8),
       value => value_to_binary(Type, Value),
       action => action_to_bool(Action),
       item_order => Order,
       match_all => MatchAll,
       match_iq => MatchIQ,
       match_message => MatchMessage,
       match_presence_in => MatchPresenceIn,
       match_presence_out => MatchPresenceOut
    }.

bool_to_action(true) -> allow;
bool_to_action(false) -> deny.

action_to_bool(allow) -> true;
action_to_bool(deny) -> false.

value_to_binary(jid, Value) -> jid:to_binary(jid:make(Value));
value_to_binary(subscription, Value) -> atom_to_binary(Value, utf8);
value_to_binary(_, Value) -> Value.

binary_to_value(<<"jid">>, Value) -> jid:to_lower(jid:from_binary(Value));
binary_to_value(<<"subscription">>, Value) -> binary_to_atom(Value, utf8);
binary_to_value(_, Value) -> Value.

null_to_binary(null) -> <<"">>;
null_to_binary(X) -> X.

null_to_list(null) -> [];
null_to_list(X) -> X.
