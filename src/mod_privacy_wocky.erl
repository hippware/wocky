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

-ifdef(TEST).
-export([default_list_items/2]).
-endif.

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/mod_privacy.hrl").

-define(DEFAULT_LIST, <<"default">>).

%%====================================================================
%% mod_privacy callbacks
%%====================================================================

-spec init(ejabberd:lserver(), [term()]) -> ok.
init(_Host, _Opts) ->
    ok.

-spec get_default_list(ejabberd:luser(), ejabberd:lserver()) ->
    {ok, {mod_privacy:list_name(), [mod_privacy:list_item()]}}.
get_default_list(LUser, LServer) ->
    {ok, {?DEFAULT_LIST, default_list_items(LUser, LServer)}}.

-spec get_list_names(ejabberd:luser(), ejabberd:lserver()) ->
    {ok, {mod_privacy:list_name(), [mod_privacy:list_name()]}}.
get_list_names(LUser, LServer) ->
    case wocky_db:select_row(LServer, privacy, [lists],
                             #{user => LUser, server => LServer}) of
        not_found ->
            {ok, {?DEFAULT_LIST, [?DEFAULT_LIST]}};
        #{lists := Lists} ->
            {ok, {?DEFAULT_LIST, [?DEFAULT_LIST |
                                  wocky_util:null_to_list(Lists)]}}
    end.

-spec get_privacy_list(ejabberd:luser(),
                       ejabberd:lserver(),
                       mod_privacy:list_name()) ->
    {error, not_found} | {ok, [mod_privacy:list_item()]}.
get_privacy_list(LUser, LServer, ?DEFAULT_LIST) ->
    {ok, default_list_items(LUser, LServer)};
get_privacy_list(LUser, LServer, Name) ->
    case get_list(LUser, LServer, Name) of
        [] ->
            {error, not_found};
        Items ->
            {ok, Items}
    end.

-spec forget_default_list(ejabberd:luser(), ejabberd:lserver()) ->
    {error, not_found}.
forget_default_list(_LUser, _LServer) ->
    % Not implemented - the default list in wocky can't be forgotten.
    {error, not_found}.

-spec set_default_list(ejabberd:luser(),
                       ejabberd:lserver(),
                       mod_privacy:list_name()) ->
    {error, not_found}.
set_default_list(_LUser, _LServer, _Name) ->
    % Not implemented - the default list in wocky can't be overridden.
    {error, not_found}.

-spec remove_privacy_list(ejabberd:luser(),
                          ejabberd:lserver(),
                          mod_privacy:list_name()) ->
    {error, conflict} | ok.
remove_privacy_list(_LUser, _LServer, ?DEFAULT_LIST) ->
    {error, conflict};
remove_privacy_list(LUser, LServer, Name) ->
    case wocky_db:select_row(LServer, privacy, [lists],
                             #{user => LUser, server => LServer}) of
        not_found ->
            ok;
        #{lists := Lists} ->
            maybe_delete_list(LUser, LServer, Name,
                              wocky_util:null_to_list(Lists)),
            ok
    end.

-spec replace_privacy_list(ejabberd:luser(),
                           ejabberd:lserver(),
                           mod_privacy:list_name(),
                           [mod_privacy:list_item()]) ->
    ok | {error, not_allowed}.
replace_privacy_list(_LUser, _LServer, ?DEFAULT_LIST, _Items) ->
    {error, not_allowed};
replace_privacy_list(LUser, LServer, Name, Items) ->
    case get_user_lists(LUser, LServer) of
        not_found ->
            ok;
        Lists ->
            maybe_delete_list(LUser, LServer, Name, Lists)
    end,
    add_list(LUser, LServer, Name, Items),
    ok.

-spec remove_user(ejabberd:luser(), ejabberd:lserver()) -> ok.
remove_user(LUser, LServer) ->
    case get_user_lists(LUser, LServer) of
        not_found ->
            ok;
        Lists ->
            lists:foreach(fun(L) -> delete_list(LUser, LServer, L) end,
                          Lists),
            ok = wocky_db:delete(LServer, privacy, all,
                                 #{user => LUser, server => LServer})
    end.

%%====================================================================
%% Helpers
%%====================================================================

% See github.com/hippware/tr-wiki/wiki/Roster%2C-presence%2C-follow-and-friends
default_list_items(LUser, LServer) ->
    BaseRecord = #listitem{match_presence_out = true,
                           match_message = true
                          },
    [
     %% Always allow messages and presences from yourself
     BaseRecord#listitem{type = jid,
                         value = jid:to_lower({LUser, LServer, <<"">>}),
                         action = allow, order = 5},

     %% Block the __no_presence__ and __blocked groups
     BaseRecord#listitem{type = group, value = <<"__no_presence__">>,
                         action = deny, order = 10},
     BaseRecord#listitem{type = group, value = <<"__blocked__">>,
                         action = deny, order = 15},

     %% Allow messages and presences between friends
     BaseRecord#listitem{type = subscription, value = both,
                         action = allow, order = 20},

     %% Allow messages from followee to follower
     #listitem{type = subscription, value = to,
               action = allow, order = 30,
               match_message = true},

     %% Deny everything else
     BaseRecord#listitem{action = deny, order = 100}
    ].

get_user_lists(LUser, LServer) ->
    wocky_util:null_to_list(
      wocky_db:select_one(LServer, privacy, lists,
                          #{user => LUser, server => LServer})).

get_list(LUser, LServer, Name) ->
    Rows = wocky_db:select(LServer, privacy_item, all,
                           #{user => LUser, server => LServer, list => Name}),
    [row_to_item(R) || R <- Rows].

maybe_delete_list(LUser, LServer, Name, Lists) ->
    case lists:member(Name, Lists) of
        false ->
            ok;
        true ->
            delete_list(LUser, LServer, Name)
    end.

delete_list(LUser, LServer, Name) ->
    ok = wocky_db:delete(LServer, privacy_item, all,
                         #{user => LUser, server => LServer, list => Name}),
    delete_list_from_user(LUser, LServer, Name).

add_list(LUser, LServer, Name, Items) ->
    Rows = [item_to_row(LUser, LServer, Name, I) || I <- Items],
    lists:foreach(fun(R) -> ok = wocky_db:insert(LServer, privacy_item, R) end,
                  Rows),
    add_list_to_user(LUser, LServer, Name).

add_list_to_user(LUser, LServer, Name) ->
    modify_user_lists(LUser, LServer, Name, "+").
delete_list_from_user(LUser, LServer, Name) ->
    modify_user_lists(LUser, LServer, Name, "-").

modify_user_lists(LUser, LServer, Name, Op) ->
    Q = ["UPDATE privacy SET lists = lists ", Op,
         " ? WHERE user = ? AND server = ?"],
    V = #{user => LUser,
          server => LServer,
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

item_to_row(LUser, LServer, List,
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
       user => LUser,
       server => LServer,
       list => List,
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
