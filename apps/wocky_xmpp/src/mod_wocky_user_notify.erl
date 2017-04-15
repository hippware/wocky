%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module implementing user field change notification
%%%
-module(mod_wocky_user_notify).

-behaviour(gen_mod).

-compile({parse_transform, do}).
-compile({parse_transform, cut}).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").
-include("wocky.hrl").
-include("wocky_roster.hrl").

%% gen_mod handlers
-export([start/2, stop/1]).

%%%===================================================================
%%% gen_mod handlers
%%%===================================================================

start(Host, _Opts) ->
    ejabberd_hooks:add(wocky_user_updated, Host,
                       fun user_updated/2, 50).

stop(Host) ->
    ejabberd_hooks:delete(wocky_user_updated, Host,
                          fun user_updated/2, 50).

%%%===================================================================
%%% Roster update hook handler
%%%===================================================================

-spec user_updated(ejabberd:luser(), ejabberd:lserver()) -> ok.
user_updated(LUser, LServer) ->
    User = ?wocky_user:find(LUser),
    WithContact = wocky_db_roster:users_with_contact(
                    jid:make(LUser, LServer, <<>>)),
    lists:foreach(notify_user_update(User, _), WithContact).

notify_user_update(nil, _) -> ok;
notify_user_update(Item = #{id := User, server := Server}, WithContact) ->
    ejabberd_router:route(jid:make(User, Server, <<>>),
                          WithContact,
                          user_change_packet(Item)).

user_change_packet(Item) ->
    #xmlel{name = <<"message">>,
           attrs = [{<<"type">>, <<"headline">>}],
           children = [make_user_changed_el(Item)]}.

make_user_changed_el(Item) ->
    #xmlel{name = <<"user-changed">>,
           attrs = [{<<"xmlns">>, ?NS_USER}],
           children = [make_item_el(Item)]}.

make_item_el(#{id := User, server := Server}) ->
    #xmlel{name = <<"item">>,
           attrs = [{<<"jid">>, jid:to_binary(jid:make(User, Server, <<>>))}]}.
