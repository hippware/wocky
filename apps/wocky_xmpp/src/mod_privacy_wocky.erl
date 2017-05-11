%%% @copyright 2016+ Hippware, Inc.
%%% @doc Wocky backend for mod_privacy
-module(mod_privacy_wocky).

-include("wocky.hrl").
-include_lib("ejabberd/include/mod_privacy.hrl").

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

-define(DEFAULT_LIST, <<"default">>).


%%====================================================================
%% mod_privacy callbacks
%%====================================================================

-spec init(ejabberd:lserver(), [term()]) -> ok.
init(Host, Opts) ->
    mod_privacy_odbc:init(Host, Opts).

-spec get_default_list(ejabberd:luser(), ejabberd:lserver()) ->
    {ok, {mod_privacy:list_name(), [mod_privacy:list_item()]}}.
get_default_list(LUser, LServer) ->
    {ok, {?DEFAULT_LIST, default_list_items(LUser, LServer)}}.

-spec get_list_names(ejabberd:luser(), ejabberd:lserver()) ->
    {ok, {mod_privacy:list_name(), [mod_privacy:list_name()]}}.
get_list_names(LUser, LServer) ->
    {ok, {_Default, List}} = mod_privacy_odbc:get_list_names(LUser, LServer),
    {ok, {?DEFAULT_LIST, [?DEFAULT_LIST | List]}}.

-spec get_privacy_list(ejabberd:luser(),
                       ejabberd:lserver(),
                       mod_privacy:list_name()) ->
    {error, not_found} | {ok, [mod_privacy:list_item()]}.
get_privacy_list(LUser, LServer, ?DEFAULT_LIST) ->
    {ok, default_list_items(LUser, LServer)};
get_privacy_list(LUser, LServer, Name) ->
    mod_privacy_odbc:get_privacy_list(LUser, LServer, Name).

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
    mod_privacy_odbc:remove_privacy_list(LUser, LServer, Name).

-spec replace_privacy_list(ejabberd:luser(),
                           ejabberd:lserver(),
                           mod_privacy:list_name(),
                           [mod_privacy:list_item()]) ->
    ok | {error, not_allowed}.
replace_privacy_list(_LUser, _LServer, ?DEFAULT_LIST, _Items) ->
    {error, not_allowed};
replace_privacy_list(LUser, LServer, Name, Items) ->
    mod_privacy_odbc:replace_privacy_list(LUser, LServer, Name, Items).

-spec remove_user(ejabberd:luser(), ejabberd:lserver()) -> ok.
remove_user(LUser, LServer) ->
    {updated, _} = mod_privacy_odbc:remove_user(LUser, LServer),
    ok.

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
