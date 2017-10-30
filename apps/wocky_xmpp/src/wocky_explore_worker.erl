-module(wocky_explore_worker).

-compile({parse_transform, cut}).

-include("wocky.hrl").

-export([start/6, start_link/1, explore/6]).

-spec start(float(), float(), float(), ejabberd:jid(), pos_integer(), binary())
-> {ok, pid()}.
start(Lat, Lon, Radius, FromJID, Limit, ID) ->
    supervisor:start_child(wocky_explore_worker_sup,
                           [[Lat, Lon, Radius, FromJID, Limit, ID]]).

% To be called by wocky_explore_worker_sup:
start_link(Args) ->
    {ok, spawn_link(?MODULE, explore, Args)}.

explore(Lat, Lon, Radius, FromJID, Limit, ID) ->
    FromUser = ?wocky_user:get_by_jid(FromJID),
    ?wocky_geosearch:explore_nearby(?wocky_geo_utils:point(Lat, Lon),
                                    Radius, FromUser, Limit,
                                    bot_found(ID, FromUser, FromJID, _)).

bot_found(ID, _FromUser, FromJID, no_more_results) ->
    send_explore_message(ID, FromJID,
                         #xmlel{name = <<"no-more-bots">>});
bot_found(ID, _FromUser, FromJID, result_limit_reached) ->
    send_explore_message(ID, FromJID,
                         #xmlel{name = <<"bot-limit-reached">>});
bot_found(ID, _FromUser, FromJID, max_explore_time) ->
    send_explore_message(ID, FromJID,
                         #xmlel{name = <<"search-time-limit-reached">>});
bot_found(ID, FromUser, FromJID, Bot) ->
    {ok, BotStanza} = mod_wocky_bot:make_bot_el(Bot, FromUser),
    send_explore_message(ID, FromJID, BotStanza).

send_explore_message(ID, FromJID, Child) ->
    Message =
    #xmlel{name = <<"message">>,
           attrs = [{<<"type">>, <<"headline">>}],
           children = [#xmlel{name = <<"explore-nearby-result">>,
                              attrs = [{<<"id">>, ID},
                                       {<<"xmlns">>, ?NS_BOT}],
                              children = [Child]}]},

    ejabberd_router:route(jid:make(<<>>, wocky_xmpp_app:server(), <<>>),
                          FromJID,
                          Message).
