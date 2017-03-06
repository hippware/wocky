%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module implementing Wocky bots - user management operations
%%% See https://github.com/hippware/tr-wiki/wiki/Bot
%%%
-module(wocky_bot_users).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").
-include("wocky.hrl").
-include("wocky_bot.hrl").
-include("wocky_roster.hrl").

-compile({parse_transform, do}).
-compile({parse_transform, cut}).

-export([handle_share/3,
         notify_new_viewers/4]).

%%%===================================================================
%%% Action - bot shared
%%%===================================================================

handle_share(_From, _To, error) -> drop;
handle_share(From, To, BotJID = #jid{lserver = Server}) ->
    ID = wocky_bot_util:get_id_from_jid(BotJID),
    Result = do([error_m ||
                 wocky_bot_util:check_access(Server, ID, From),
                 check_can_share(Server, ID, From),
                 wocky_db_bot:add_share(From, To, BotJID),
                 send_notification(From, To)
                ]),
    case Result of
        ok -> ok;
        _ -> drop
    end.

check_can_share(Server, ID, Sharer) ->
    case wocky_db_bot:is_public(Server, ID) of
        true ->
            ok;
        false ->
            Owner = wocky_db_bot:owner(Server, ID),
            case jid:are_bare_equal(Owner, Sharer) of
                true -> ok;
                false -> {error, cant_share}
            end
    end.

send_notification(#jid{luser = User, lserver = Server} = From, To) ->
    Handle = case wocky_db_user:get_handle(User, Server) of
                 not_found -> <<"Someone">>;
                 H -> H
             end,
    Body = <<Handle/binary, " shared a bot with you!">>,
    wocky_notification_handler:notify_message(To, From, Body).

%%%===================================================================
%%% Access change notifications
%%%===================================================================

-spec notify_new_viewers(Server :: ejabberd:lserver(),
                         ID :: wocky_db:id(),
                         OldPublic :: boolean() | none,
                         NewPublic :: boolean()) -> ok.

% Unchanged visibility
notify_new_viewers(_, _, Vis, Vis) -> ok;

% New visibility is public - notify friends and followers
notify_new_viewers(Server, ID, _, true) ->
    Owner = wocky_db_bot:owner(Server, ID),
    notify_new_viewers(Server, ID, get_friends_and_followers(Owner));

% Any other case does not generate notification
notify_new_viewers(_, _, _, _) -> ok.

notify_new_viewers(Server, ID, NewViewers) ->
    lists:foreach(notify_new_viewer(Server, ID, _), NewViewers).

notify_new_viewer(Server, ID, Viewer) ->
    ejabberd_router:route(jid:make(<<>>, Server, <<>>),
                          Viewer,
                          bot_visible_stanza(Server, ID)).

bot_visible_stanza(Server, ID) ->
    #xmlel{name = <<"message">>,
           attrs = [{<<"type">>, <<"headline">>}],
           children = [#xmlel{name = <<"bot">>,
                              attrs = [{<<"xmlns">>, ?NS_BOT}],
                              children = bot_visible_children(Server, ID)}]}.

bot_visible_children(Server, ID) ->
    [cdata_el(<<"jid">>, jid:to_binary(
                           wocky_bot_util:make_jid(Server, ID))),
     cdata_el(<<"id">>, ID),
     cdata_el(<<"server">>, wocky_app:server()),
     cdata_el(<<"action">>, <<"show">>)].

cdata_el(Name, Value) ->
    #xmlel{name = Name,
           children = [#xmlcdata{content = Value}]}.

get_friends_and_followers(#jid{luser = LUser, lserver = LServer}) ->
    [roster_to_jid(R) || R <- wocky_db_roster:followers(LUser, LServer)].

roster_to_jid(#wocky_roster{contact_jid = SimpleJID}) ->
    jid:make(SimpleJID).
