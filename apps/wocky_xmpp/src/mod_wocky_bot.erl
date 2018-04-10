%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module implementing Wocky bots
%%% See https://github.com/hippware/tr-wiki/wiki/Bot
%%%
-module(mod_wocky_bot).

-compile({parse_transform, do}).
-compile({parse_transform, cut}).
-compile({parse_transform, fun_chain}).

-include("wocky.hrl").
-include("wocky_bot.hrl").

-behaviour(gen_mod).
-behaviour(wocky_access_manager).

%% gen_mod handlers
-export([start/2, stop/1, deps/2]).

%% IQ handler
-export([handle_iq/3]).

%% Access manager callback
-export([check_access/3]).

%% Other functions
-export([make_bot_el/2,
         make_bot_els/2]).

-type loc() :: {float(), float()}.
-type tags() :: [binary()].

-type field_type() :: string | int | float | bool |
                      geoloc | jid | timestamp | tags | url_list.
-type field_types() :: field_type() | [field_type()].
-type value_type() :: nil | binary() | integer() | float() | boolean()
                      | loc() | jid() | tags() | ?datetime:t().

-record(field, {
          name :: binary(),
          type :: field_types(),
          value :: value_type()
         }).

-define(PACKET_FILTER_PRIORITY, 40).

-define(DEFAULT_SORTING, {asc, updated_at}).

%%%===================================================================
%%% gen_mod handlers
%%%===================================================================

start(Host, _Opts) ->
    ?wocky_xmpp_bot_callbacks:register(),
    ?wocky_xmpp_bot_share_callbacks:register(),
    wocky_watcher:register(bot, Host),
    wocky_publishing_handler:register(<<"bot">>, wocky_bot_publishing),

    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_BOT,
                                  ?MODULE, handle_iq, parallel),
    mod_disco:register_feature(Host, ?NS_BOT),
    ejabberd_hooks:add(filter_local_packet, Host,
                       fun filter_local_packet_hook/1,
                       ?PACKET_FILTER_PRIORITY),
    wocky_access_manager:register(<<"bot">>, ?MODULE).

stop(Host) ->
    mod_disco:unregister_feature(Host, ?NS_BOT),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_BOT),
    ejabberd_hooks:delete(filter_local_packet, Host,
                          fun filter_local_packet_hook/1,
                          ?PACKET_FILTER_PRIORITY),
    wocky_access_manager:unregister(<<"bot">>, ?MODULE),

    wocky_publishing_handler:unregister(<<"bot">>, wocky_bot_publishing),
    wocky_watcher:unregister(bot, Host).

deps(_Host, _Opts) ->
    [{mod_wocky_publishing, hard}].

%%%===================================================================
%%% Event handler
%%%===================================================================

-spec handle_iq(From :: ejabberd:jid(),
                To :: ejabberd:jid(),
                IQ :: iq()) -> iq().
handle_iq(From, To, IQ) ->
    #iq{type = Type, sub_el = #xmlel{name = Op, attrs = Attrs}} = IQ,
    case handle_iq_type(From, To, Type, Op, Attrs, IQ) of
        {ok, SubEl} ->
            IQ#iq{type = result, sub_el = SubEl};

        {error, #{'valid?' := false} = Changeset} ->
            Error = handle_validation_errors(?wocky_errors:to_map(Changeset)),
            wocky_util:make_error_iq_response(IQ, Error);

        {error, Error} ->
            wocky_util:make_error_iq_response(IQ, Error)
    end.

% New ID
handle_iq_type(From, _To, set, <<"new-id">>, _Attrs, _IQ) ->
    handle_preallocate(From);

% Create
handle_iq_type(From, _To, set, <<"create">>, _Attrs, IQ) ->
    #iq{sub_el = #xmlel{children = Children}} = IQ,
    handle_create(From, Children);

% Delete
handle_iq_type(From, _To, set, <<"delete">>, Attrs, IQ) ->
    handle_owner_action(delete, From, Attrs, false, IQ);

% Retrieve owned bots
handle_iq_type(From, To, get, Name, Attrs, IQ)
  %% We want 'bot' for retrieving a single bot and 'bots' for a list of bots.
  %% The documentation is inconsistent, so to avoid breaking anything, we will
  %% accept either.
  when Name =:= <<"bot">> orelse Name =:= <<"bots">> ->
    Node = wocky_xml:get_attr(<<"node">>, Attrs),

    Location = get_location_from_attrs(Attrs),    % DEPRECATED
    User = wocky_xml:get_attr(<<"user">>, Attrs), % DEPRECATED

    Owner = wocky_xml:act_on_subel(<<"owner">>, IQ#iq.sub_el,
                                   wocky_xml:get_attr(<<"jid">>, _)),

    ExploreNearby = wocky_xml:get_subel(<<"explore-nearby">>, IQ#iq.sub_el),

    case {Node, Location, User, Owner, ExploreNearby} of
        {{ok, _Node}, _, _, _, _} ->
            handle_access_action(get_bot, From, To, Attrs, false, IQ);
        {_, {ok, {Lat, Lon}}, _, _, _} ->
            get_bots_near_location(From, Lat, Lon);
        {_, _, {ok, UserBin}, _, _} ->
            get_bots_for_owner(From, IQ, UserBin);
        {_, _, _, {ok, OwnerBin}, _} ->
            get_bots_for_owner(From, IQ, OwnerBin);
        {_, _, _, _, {ok, ExploreNearbyEl}} ->
            explore_nearby(From, IQ, ExploreNearbyEl);
        _ ->
            {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Invalid query">>)}
    end;

% Retrieve subscribed bots
handle_iq_type(From, To, get, Name, _Attrs, IQ)
  when Name =:= <<"following">> orelse %% Backwards compatability only
       Name =:= <<"subscribed">> ->
    handle_subscribed(From, To, IQ);

% Update
% Old style
handle_iq_type(From, _To, set, <<"fields">>, Attrs, IQ) ->
    handle_owner_action(update, From, Attrs, false, IQ);
% New style
handle_iq_type(From, _To, set, <<"update">>, Attrs, IQ) ->
    handle_owner_action(update, From, Attrs, false, IQ);

% Subscribe
handle_iq_type(From, To, set, <<"subscribe">>, Attrs, IQ) ->
    handle_access_action(subscribe, From, To, Attrs, false, IQ);

% Unsubscribe
handle_iq_type(From, To, set, <<"unsubscribe">>, Attrs, _IQ) ->
    handle_unsubscribe(From, To, Attrs);

% Retrieve subscribers
handle_iq_type(From, _To, get, <<"subscribers">>, Attrs, IQ) ->
    handle_owner_action(subscribers, From, Attrs, false, IQ);

% Retrieve item(s)
handle_iq_type(From, To, get, <<"query">>, Attrs, IQ) ->
    handle_access_action(item_query, From, To, Attrs, true, IQ);

% Publish an item
handle_iq_type(From, To, set, <<"publish">>, Attrs, IQ) ->
    handle_access_action(publish, From, To, Attrs, true, IQ);

% Delete an item
handle_iq_type(From, To, set, <<"retract">>, Attrs, IQ) ->
    handle_access_action(retract, From, To, Attrs, true, IQ);

% Get a list of images from items on the bot
handle_iq_type(From, To, get, <<"item_images">>, Attrs, IQ) ->
    handle_access_action(item_images, From, To, Attrs, false, IQ);

% Get a list of guests on the bot
handle_iq_type(From, _To, get, <<"guests">>, Attrs, IQ) ->
    handle_owner_action(guests, From, Attrs, false, IQ);

% Get a list of visitors to the bot
handle_iq_type(From, _To, get, <<"visitors">>, Attrs, IQ) ->
    handle_guest_action(visitors, From, Attrs, IQ);

handle_iq_type(_From, _To, _Type, _Op, _Attrs, _IQ) ->
    {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Invalid query">>)}.


%%%===================================================================
%%% Actions that only the owner may perform
%%%===================================================================

handle_owner_action(Action, From, Attrs, AllowPending, IQ) ->
    do([error_m ||
           Bot <- wocky_bot_util:get_bot_from_node(Attrs, AllowPending),
           wocky_bot_util:check_owner(Bot, From),
           perform_owner_action(Action, Bot, From, IQ)
       ]).

perform_owner_action(update, Bot, _From, IQ) ->
    #iq{sub_el = #xmlel{children = Children}} = IQ,
    do([error_m ||
        Fields <- get_fields(Children),
        FieldsMap = normalise_fields(Fields),
        ?wocky_bot:update(Bot, FieldsMap),
        {ok, []}
       ]);

perform_owner_action(delete, Bot, _From, _IQ) ->
    ?wocky_bot:delete(Bot),
    {ok, []};

perform_owner_action(subscribers, Bot, _From, IQ) ->
    wocky_bot_subscription:retrieve_subscribers(Bot, IQ);

perform_owner_action(guests,
                     #{guests_count := Count, guests_hash := Hash} = Bot,
                     _From, IQ) ->
    do([error_m ||
        RSMIn <- rsm_util:get_rsm(IQ),
        Sorting <- get_sorting(IQ),
        Query <- {ok, ?wocky_bot:guests_query(Bot)},
        {Guests, RSMOut} <- {ok, ?wocky_rsm_helper:rsm_query(
                                    RSMIn, Query, id, Sorting)},
        {ok, users_result(<<"guests">>, make_guests(Guests),
                          Count, Hash, RSMOut)}
       ]).

%%%===================================================================
%%% Actions that require the user to be a guest of the bot
%%%===================================================================

handle_guest_action(Action, FromJID, Attrs, IQ) ->
    do([error_m ||
           Bot <- wocky_bot_util:get_bot_from_node(Attrs),
           From <- wocky_bot_util:get_user_from_jid(FromJID),
           wocky_bot_util:check_guest(From, Bot),
           perform_guest_action(Action, Bot, IQ)
       ]).

perform_guest_action(visitors,
                     #{visitors_count := Count, visitors_hash := Hash} = Bot,
                     IQ) ->
    do([error_m ||
        RSMIn <- rsm_util:get_rsm(IQ),
        Sorting <- get_sorting(IQ),
        Query <- {ok, ?wocky_bot:visitors_query(Bot)},
        {Visitors, RSMOut} <- {ok, ?wocky_rsm_helper:rsm_query(
                                      RSMIn, Query, id, Sorting)},
        {ok, users_result(<<"visitors">>, make_visitors(Visitors),
                          Count, Hash, RSMOut)}
       ]).

%%%===================================================================
%%% Actions that require the user to have access to the bot
%%%===================================================================

handle_access_action(Action, FromJID, ToJID, Attrs, AllowPending, IQ) ->
    do([error_m ||
           Bot <- wocky_bot_util:get_bot_from_node(Attrs, AllowPending),
           From <- wocky_bot_util:get_user_from_jid(FromJID),
           wocky_bot_util:check_access(From, Bot),
           perform_access_action(Action, Bot, From, ToJID, IQ)
       ]).

perform_access_action(get_bot, Bot, From, _ToJID, _IQ) ->
    do([error_m ||
           BotEl <- make_bot_el(Bot, From),
           {ok, BotEl}
       ]);

perform_access_action(item_query, Bot, #{id := FromID}, _ToJID, IQ) ->
    wocky_bot_item:query(Bot, IQ, FromID);

perform_access_action(item_images, Bot, FromUser, _ToJID, IQ) ->
    wocky_bot_item:query_images(Bot, IQ, FromUser);

perform_access_action(subscribe, Bot, From, _ToJID, #iq{sub_el = SubEl}) ->
    do([error_m ||
            GuestBin <- wocky_xml:get_subel_cdata(
                          <<"geofence">>, SubEl, <<"false">>),
            Guest <- read_bool(GuestBin),
            wocky_bot_subscription:subscribe(From, Bot, Guest)
       ]);

perform_access_action(publish, Bot, From, ToJID, #iq{sub_el = SubEl}) ->
    wocky_bot_item:publish(Bot, From, ToJID, SubEl);

perform_access_action(retract, Bot, From, ToJID, #iq{sub_el = SubEl}) ->
    wocky_bot_item:retract(Bot, From, ToJID, SubEl).

%%%===================================================================
%%% Action - new-id
%%%===================================================================

handle_preallocate(#jid{luser = UserID}) ->
    Server = wocky_xmpp_app:server(),
    #{id := ID} = ?wocky_bot:preallocate(UserID, Server),
    {ok, make_new_id_stanza(ID)}.

%%%===================================================================
%%% Action - create
%%%===================================================================

handle_create(From, Children) ->
    Server = wocky_xmpp_app:server(),
    do([error_m ||
        Fields <- get_fields(Children),
        {ID, PendingBot} <- get_pending_id_and_bot(Fields),
        wocky_bot_util:check_owner(ID, From),
        User <- wocky_bot_util:get_user_from_jid(From),
        Fields2 <- add_server(Fields, Server),
        Fields3 <- add_defaults(Fields2),
        check_required_fields(Fields3, required_fields()),
        FieldsMap = normalise_fields(Fields3),
        Bot <- create_bot(ID, PendingBot, User, FieldsMap),
        %% See note below:
        ?wocky_bot:subscribe(Bot, User, maps:get(geofence, Bot)),
        FinalBot <- {ok, ?wocky_bot:get(ID)},
        BotEl <- make_bot_el(FinalBot, User),
        {ok, BotEl}
       ]).

%% Note: We do the subscription in handle_create AND in
%% the bot callback system because we're returning the bot
%% immediately and need to have our owner show up as a
%% subscriber.
%% However if the bot is created through a different channel,
%% we also want the subscription to be created so we need
%% that system too.

add_server(Fields, Server) ->
    {ok, [#field{name = <<"server">>, type = string, value = Server} | Fields]}.

add_defaults(Fields) ->
    {ok, lists:foldl(fun maybe_add_default/2, Fields, optional_fields())}.

maybe_add_default(#field{name = Name, type = Type, value = Default}, Fields) ->
    case lists:keyfind(Name, #field.name, Fields) of
        false ->
            [#field{name = Name, type = Type, value = Default} | Fields];
        _ ->
            Fields
    end.

get_pending_id_and_bot(Fields) ->
    case lists:keyfind(<<"id">>, #field.name, Fields) of
        false -> {ok, {?wocky_id:new(), nil}};
        #field{value = ID} -> check_pending_id(ID)
    end.

check_pending_id(ID) ->
    case ?wocky_repo:get(?wocky_bot, ID) of
        Bot = #{pending := true} -> {ok, {ID, Bot}};
        _ -> {error, ?ERR_ITEM_NOT_FOUND}
    end.

create_bot(ID, nil, #{id := UserID}, Fields) ->
    ?wocky_bot:insert(Fields#{id => ID, user_id => UserID});
create_bot(_ID, PendingBot, _User, Fields) ->
    ?wocky_bot:update(PendingBot, Fields).

%%%===================================================================
%%% Actions - get/subscribed
%%%===================================================================

handle_subscribed(From, _To, IQ) ->
    do([error_m ||
        User <- wocky_bot_util:get_user_from_jid(From),
        RSMIn <- rsm_util:get_rsm(IQ),
        Sorting <- get_sorting(IQ),
        {SubscribedBots, RSMOut} <- get_subscribed_bots(User, Sorting, RSMIn),
        {ok, users_bots_result(SubscribedBots, User, RSMOut)}
       ]).

handle_unsubscribe(From, _To, Attrs) ->
    do([error_m ||
        User <- wocky_bot_util:get_user_from_jid(From),
        Bot <- wocky_bot_util:get_bot_from_node(Attrs),
        wocky_bot_subscription:unsubscribe(User, Bot)
       ]).

get_location_from_attrs(Attrs) ->
    do([error_m ||
        Lat <- wocky_xml:get_attr(<<"lat">>, Attrs),
        Lon <- wocky_xml:get_attr(<<"lon">>, Attrs),
        {ok, {Lat, Lon}}
       ]).

% Old-style (Algolia-based) explore-nearby
get_bots_near_location(From, Lat, Lon) ->
    case ?wocky_index:geosearch(Lat, Lon) of
        {ok, AllBots} ->
            User = ?wocky_user:get_by_jid(From),
            VisibleBots = lists:filter(
                            ?wocky_user:'searchable?'(User, _), AllBots),
            {ok, make_geosearch_result(VisibleBots, User)};
        {error, indexing_disabled} ->
            {error,
             ?ERRT_FEATURE_NOT_IMPLEMENTED(
                ?MYLANG, <<"Index search is not configured on this server">>)}
    end.

get_subscribed_bots(QueryingUser, {asc, distance, Lat, Lon}, RSMIn) ->
    {ok,
     ?wocky_geosearch:subscribed_distance_query(Lat, Lon,
                                                maps:get(id, QueryingUser),
                                                RSMIn)};

get_subscribed_bots(User, Sorting, RSMIn) ->
    BaseQuery = ?wocky_user:subscribed_bots_query(User),
    FilteredQuery = ?wocky_bot:is_visible_query(BaseQuery, User),
    {ok, ?wocky_rsm_helper:rsm_query(RSMIn, FilteredQuery, id, Sorting)}.

make_geosearch_result(Bots, FromUser) ->
    #xmlel{name = <<"bots">>,
           attrs = [{<<"xmlns">>, ?NS_BOT}],
           children = make_geosearch_els(Bots, FromUser)}.

make_geosearch_els(Bots, FromUser) ->
    [make_geosearch_el(Bot, FromUser) || Bot <- Bots].

geosearch_el_fields() ->
    [id, server, user_id, title, image, location, radius, distance].

make_geosearch_el(Bot, FromUser) ->
    JidField = make_field(<<"jid">>, jid, ?wocky_bot:to_jid(Bot)),
    MapFields = maps:fold(fun to_field/3, [],
                          maps:with(geosearch_el_fields(), Bot)),
    RetFields = encode_fields([JidField | MapFields], FromUser),
    make_ret_stanza(RetFields).

get_bots_for_owner(From, IQ, OwnerBin) ->
    do([error_m ||
        OwnerJID <- make_jid(OwnerBin),
        Owner <- wocky_bot_util:get_user_from_jid(OwnerJID),
        RSMIn <- rsm_util:get_rsm(IQ),
        FromUser <- wocky_bot_util:get_user_from_jid(From),
        Sorting <- get_sorting(IQ),
        {FilteredBots, RSMOut} <-
            get_bots_for_owner_rsm(Owner, FromUser, Sorting, RSMIn),
        {ok, users_bots_result(FilteredBots, FromUser, RSMOut)}
       ]).

get_bots_for_owner_rsm(Owner, QueryingUser, {asc, distance, Lat, Lon}, RSMIn) ->
    {ok,
     ?wocky_geosearch:user_distance_query(Lat, Lon,
                                          maps:get(id, QueryingUser),
                                          maps:get(id, Owner),
                                          RSMIn)};

get_bots_for_owner_rsm(Owner, QueryingUser, Sorting, RSMIn) ->
    BaseQuery = ?wocky_user:owned_bots_query(Owner),
    FilteredQuery = ?wocky_bot:is_visible_query(BaseQuery, QueryingUser),
    {ok, ?wocky_rsm_helper:rsm_query(RSMIn, FilteredQuery, id, Sorting)}.

explore_nearby(From, IQ, ExploreNearby) ->
    do([error_m ||
        {Lat, Lon} <- get_location_from_attrs(ExploreNearby#xmlel.attrs),
        AreaConstraint <- get_area_constraint(ExploreNearby),
        LimitBin <- wocky_xml:get_attr(<<"limit">>, ExploreNearby),
        Limit <- wocky_util:safe_bin_to_integer(LimitBin),
        wocky_explore_worker:start(
          Lat, Lon, AreaConstraint, From, Limit, IQ#iq.id),
        {ok, []}
       ]).

get_area_constraint(ExploreNearby) ->
    case wocky_xml:get_attr(<<"radius">>, ExploreNearby) of
        {ok, RadiusBin} ->
            wocky_util:safe_bin_to_float(RadiusBin);
        _ ->
            get_lat_lon_delta(ExploreNearby)
    end.

get_lat_lon_delta(ExploreNearby) ->
    do([error_m ||
        LatDeltaBin <- wocky_xml:get_attr(<<"lat_delta">>, ExploreNearby),
        LonDeltaBin <- wocky_xml:get_attr(<<"lon_delta">>, ExploreNearby),
        LatDelta <- wocky_util:safe_bin_to_float(LatDeltaBin),
        LonDelta <- wocky_util:safe_bin_to_float(LonDeltaBin),
        {ok, ?wocky_geo_utils:point(LatDelta, LonDelta)}
       ]).

users_bots_result(Bots, FromUser, RSMOut) ->
    BotEls = make_bot_els(Bots, FromUser),
    #xmlel{name = <<"bots">>,
           attrs = [{<<"xmlns">>, ?NS_BOT}],
           children = BotEls ++ jlib:rsm_encode(RSMOut)}.

make_bot_els(Bots, FromUser) ->
    lists:map(do_make_bot_els(_, FromUser), Bots).

do_make_bot_els(Bot, FromUser) ->
    {ok, El} = make_bot_el(Bot, FromUser),
    El.


%%%===================================================================
%%% Incoming packet handler
%%%===================================================================

-type filter_packet() :: {ejabberd:jid(), ejabberd:jid(),
                          mongoose_acc:t(), jlib:xmlel()}.
-spec filter_local_packet_hook(filter_packet() | drop) ->
    filter_packet() | drop.

%% Packets to another entity which reference a bot. Passed through unless
%% they hit a condition to block them.
filter_local_packet_hook(P = {From, To, _Acc,
                              Stanza = #xmlel{name = <<"message">>,
                                              attrs = Attrs}}) ->
    Result = case xml:get_attr(<<"type">>, Attrs) of
        {value, <<"headline">>} -> handle_headline_packet(From, To, Stanza);
        _ -> ok
    end,
    case Result of
        drop ->
            reply_not_allowed(From, Stanza),
            drop;
        ok -> P
    end;

%% Ignore all other packets
filter_local_packet_hook(Other) ->
    Other.

handle_headline_packet(From, To, Stanza) ->
    case xml:get_subtag(Stanza, <<"bot">>) of
        false -> ok;
        BotStanza -> handle_bot_stanza(From, To, BotStanza)
    end.

handle_bot_stanza(From, To, BotStanza) ->
    case wocky_bot_util:bot_packet_action(BotStanza) of
        {Bot, share} ->
            wocky_bot_users:handle_share(From, To, Bot);
        {Bot, geofence_share} ->
            wocky_bot_users:handle_geofence_share(From, To, Bot);
        _ ->
            ok
    end.

%%%===================================================================
%%% Access manager callback
%%%===================================================================

-spec check_access(binary(), ejabberd:jid(), wocky_access_manager:op()) ->
    wocky_access_manager:access_result().
check_access(BotNode, ActorJID, view) ->
    BotID = ?wocky_bot:get_id_from_node(BotNode),
    R = do([error_m ||
               Bot <- wocky_bot_util:get_bot(BotID, true),
               Actor <- wocky_bot_util:get_user_from_jid(ActorJID),
               {ok, ?wocky_user:'can_access?'(Actor, Bot)}
           ]),
    allow_to_result(R);

check_access(BotNode, ActorJID, _) ->
    BotID = ?wocky_bot:get_id_from_node(BotNode),
    R = do([error_m ||
               Bot <- wocky_bot_util:get_bot(BotID, true),
               Actor <- wocky_bot_util:get_user_from_jid(ActorJID),
               {ok, ?wocky_user:'owns?'(Actor, Bot)}
           ]),
    allow_to_result(R).

allow_to_result({ok, true}) -> allow;
allow_to_result(_) -> deny.

%%%===================================================================
%%% Common helpers
%%%===================================================================

get_fields(Children) ->
    get_fields(Children, []).

get_fields(_, [{error, E} | _]) -> {error, E};
get_fields([], Acc) -> {ok, Acc};
% Old Style:
get_fields([El = #xmlel{name = <<"field">>,
                        attrs = Attrs}
            | Rest] , Acc) ->
    F = do([error_m ||
            Name <- wocky_xml:get_attr(<<"var">>, Attrs),
            TypeBin <- wocky_xml:get_attr(<<"type">>, Attrs),
            Type <- check_field(Name, TypeBin),
            Value <- old_get_field_value(Type, El),
            #field{name = Name, type = Type, value = Value}
           ]),
    get_fields(Rest, [F | Acc]);
% New Style:
get_fields([#xmlel{name = Name, children = Children} | Rest], Acc) ->
    F = do([error_m ||
            Type <- check_field(Name),
            Value <- get_field_value(Type, Children),
            #field{name = Name, type = Type, value = Value}
           ]),
    get_fields(Rest, [F | Acc]).

old_get_field_value(string, FieldEl) ->
    wocky_xml:get_subel_cdata(<<"value">>, FieldEl);
old_get_field_value(int, FieldEl) ->
    wocky_xml:act_on_subel_cdata(<<"value">>, FieldEl, fun read_integer/1);
old_get_field_value(bool, FieldEl) ->
    wocky_xml:act_on_subel_cdata(<<"value">>, FieldEl, fun read_bool/1);
old_get_field_value(float, FieldEl) ->
    wocky_xml:act_on_subel_cdata(<<"value">>, FieldEl, fun read_float/1);
old_get_field_value(geoloc, FieldEl) ->
    wocky_xml:act_on_subel(<<"geoloc">>, FieldEl, fun read_geoloc/1);
old_get_field_value(tags, FieldEl) ->
    wocky_xml:foldl_subels(FieldEl, [], fun read_tag/2).

get_field_value(string, [#xmlcdata{content = Value}]) -> {ok, Value};
get_field_value(string, []) -> {ok, <<>>};
% Remove once old interface is removed:
get_field_value([int, float], [#xmlcdata{content = Value}]) ->
    read_float(Value);
get_field_value(int, [#xmlcdata{content = Value}]) -> read_integer(Value);
get_field_value(bool, [#xmlcdata{content = Value}]) -> read_bool(Value);
get_field_value(float, [#xmlcdata{content = Value}]) -> read_float(Value);
get_field_value(geoloc, [Geoloc]) -> read_geoloc(Geoloc);
get_field_value(tags, Children) ->
    wocky_util:check_foldl(fun read_tag/2, [], Children);
get_field_value(_, _) ->
    {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Invalid field type">>)}.


read_integer(Binary) ->
    case wocky_util:safe_bin_to_integer(Binary) of
        {ok, Int} -> {ok, Int};
        {error, _} -> {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Invalid integer">>)}
    end.

read_float(Binary) ->
    case wocky_util:safe_bin_to_float(Binary) of
        {ok, Float} -> {ok, Float};
        {error, _} -> {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Invalid float">>)}
    end.

read_geoloc(GeolocEl) ->
    do([error_m ||
        wocky_xml:check_namespace(?NS_GEOLOC, GeolocEl),
        Lat <- wocky_xml:act_on_subel_cdata(
                 <<"lat">>, GeolocEl, fun read_float/1),
        Lon <- wocky_xml:act_on_subel_cdata(
                 <<"lon">>, GeolocEl, fun read_float/1),
        {ok, {Lat, Lon}}
       ]).

read_tag(#xmlel{name = <<"tag">>,
                children = [#xmlcdata{content = Val}]}, Acc) ->
    {ok, [Val | Acc]};
read_tag(_Element, _Acc) ->
    {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Invalid tag">>)}.

read_bool(Binary) ->
    case wocky_util:safe_bin_to_boolean(Binary) of
        {ok, Bool} -> {ok, Bool};
        {error, _} -> {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Invalid boolean">>)}
    end.

get_sorting(#iq{sub_el = SubEl}) ->
    case wocky_xml:get_subel(<<"sort">>, SubEl) of
        {ok, SortEl} -> parse_sorting(SortEl);
        {error, _} -> {ok, ?DEFAULT_SORTING}
    end.

parse_sorting(SortEl) ->
    do([error_m ||
        By <- wocky_xml:get_attr(<<"by">>, SortEl#xmlel.attrs),
        ByResult <- check_by(By),
        Dir <- wocky_xml:get_attr(<<"direction">>, SortEl#xmlel.attrs),
        DirResult <- check_dir(Dir, ByResult),
        maybe_add_near(SortEl, ByResult, DirResult)
       ]).

check_by(<<"created">>) ->  {ok, created_at};
check_by(<<"updated">>) ->  {ok, updated_at};
check_by(<<"title">>) ->    {ok, title};
check_by(<<"distance">>) -> {ok, distance};
check_by(_) ->
    {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Invalid sort field">>)}.

check_dir(<<"asc">>, _) -> {ok, asc};
check_dir(<<"desc">>, By) when By =/= distance -> {ok, desc};
check_dir(_, _) ->
    {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Invalid sort direction">>)}.

maybe_add_near(SortEl, distance, asc) ->
    do([error_m ||
        Near <- wocky_xml:get_subel(<<"near">>, SortEl),
        Lat <- wocky_xml:get_attr(<<"lat">>, Near),
        Lon <- wocky_xml:get_attr(<<"lon">>, Near),
        {ok, {asc, distance, Lat, Lon}}
       ]);
maybe_add_near(_, By, Dir) -> {ok, {Dir, By}}.

check_required_fields(_Fields, []) -> ok;
check_required_fields(Fields, [#field{name = Name} | Rest]) ->
    case lists:any(fun(#field{name = FName}) -> FName =:= Name end, Fields) of
        true ->
            check_required_fields(Fields, Rest);
        false ->
            {error, ?ERRT_BAD_REQUEST(?MYLANG,
                                      <<"Missing ", Name/binary, " field">>)}
    end.

check_field(Name) ->
    case lists:keyfind(Name, #field.name, create_fields()) of
        #field{type = ExpectedType} ->
            {ok, ExpectedType};
        false ->
            {error, ?ERRT_BAD_REQUEST(
                       ?MYLANG, <<"Invalid field ", Name/binary>>)}
    end.

check_field(Name, TypeBin) ->
    case lists:keyfind(Name, #field.name, create_fields()) of
        #field{type = ExpectedType} ->
            check_field_type(Name, TypeBin, ExpectedType);
        false ->
            {error, ?ERRT_BAD_REQUEST(
                       ?MYLANG, <<"Invalid field ", Name/binary>>)}
    end.

check_field_type(Name, TypeBin, [ExpectedType | Rest]) ->
    case check_field_type(Name, TypeBin, ExpectedType) of
        {ok, Result} -> {ok, Result};
        {error, E} when Rest =:= [] -> {error, E};
        {error, _} -> check_field_type(Name, TypeBin, Rest)
    end;

check_field_type(Name, TypeBin, ExpectedType) ->
    ExpectedTypeBin = atom_to_binary(ExpectedType, utf8),
    case ExpectedTypeBin of
        TypeBin ->
            {ok, ExpectedType};
        OtherType ->
            {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Invalid type '",
                                                 TypeBin/binary,
                                                 "' on field ", Name/binary,
                                                 ". Expected: '",
                                                 OtherType/binary, "'">>)}
    end.

required_fields() ->
    %% Name,                    Type,   Default
    [field(<<"title">>,         string, <<>>),
     field(<<"location">>,      geoloc, <<>>),
     %% Radius also takes an int for backwards compatibility.
     field(<<"radius">>,        [int, float], 100.0)].

optional_fields() ->
    [field(<<"id">>,            string, <<>>),
     field(<<"description">>,   string, <<>>),
     field(<<"shortname">>,     string, <<>>),
     field(<<"image">>,         string, <<>>),
     field(<<"type">>,          string, <<>>),
     field(<<"address">>,       string, <<>>),
     field(<<"address_data">>,  string, <<>>),
     field(<<"visibility">>,    int,    ?WOCKY_BOT_VIS_OWNER),
     field(<<"public">>,        bool,   false),
     field(<<"tags">>,          tags,   []),
     field(<<"geofence">>,      bool,   false)].

output_only_fields() ->
    [field(<<"server">>,        string, <<>>),
     field(<<"owner">>,         jid,    <<>>),
     field(<<"updated">>,       timestamp, <<>>),
     field(<<"subscribed">>,    bool,   false),
     field(<<"distance">>,      float,  0.0)
    ].

create_fields() -> required_fields() ++ optional_fields().
output_fields() -> required_fields() ++ optional_fields()
                   ++ output_only_fields().

field(Name, Type, Value) ->
    #field{name = Name, type = Type, value = Value}.

handle_validation_errors(Errors) ->
    not_valid(?wocky_errors:render_errors(Errors)).

not_valid(Message) ->
    El = #xmlel{children = Children} =
        jlib:stanza_errort(<<"409">>, <<"modify">>, <<"undefined-condition">>,
                           ?MYLANG, iolist_to_binary(Message)),
    El#xmlel{children = [#xmlel{name = <<"not-valid">>,
                                attrs = [{<<"xmlns">>, ?NS_ERRORS}]}
                         | Children]}.

%% TODO
%% name_conflict_error() ->
%%     BaseStanza = jlib:stanza_error(<<"409">>, <<"modify">>,
%%                                    <<"undefined-condition">>),
%%     ConflictEl = #xmlel{name = <<"conflict">>,
%%                         attrs = [{<<"xmlns">>, ?NS_ERRORS}],
%%                         children = [#xmlel{name = <<"field">>,
%%                                            attrs = [{<<"var">>,
%%                                                      <<"shortname">>}]}]},
%%     BaseStanza#xmlel{children = [ConflictEl | BaseStanza#xmlel.children]}.

normalise_fields(Fields) ->
    lists:foldl(fun normalise_field/2, #{}, Fields).

normalise_field(#field{type = geoloc, value = {Lat, Lon}}, Acc) ->
    Acc#{location => ?wocky_geo_utils:point(Lat, Lon)};
normalise_field(#field{type = jid, value = #jid{luser = UserID}}, Acc) ->
    Acc#{user_id => UserID};
normalise_field(#field{name = <<"visibility">>, value = 100}, Acc) ->
    Acc#{public => true};
normalise_field(#field{name = <<"visibility">>}, Acc) ->
    Acc#{public => false};
normalise_field(#field{name = N, value = V}, Acc) ->
    Acc#{binary_to_existing_atom(N, utf8) => V}.

make_bot_el(Bot, FromUser) ->
    RetFields = make_ret_elements(Bot, FromUser),
    {ok, make_ret_stanza(RetFields)}.

make_ret_elements(Bot, FromUser) ->
    DynamicFields = dynamic_fields(Bot, FromUser),
    Fields = maps:fold(fun to_field/3, [], Bot),
    encode_fields(Fields ++ DynamicFields, FromUser).

dynamic_fields(Bot, FromUser) ->
    TotalItems = ?wocky_item:get_count(Bot),
    ImageItems = ?wocky_item:get_image_count(Bot),
    SubscribeState = ?wocky_bot:subscription(Bot, FromUser),
    [make_field(<<"jid">>, jid, ?wocky_bot:to_jid(Bot)),
     make_field(<<"total_items">>, int, TotalItems),
     make_field(<<"image_items">>, int, ImageItems),
     make_field(<<"subscribed">>, bool, SubscribeState =/= nil),
     make_field(<<"guest">>, bool,
                SubscribeState =:= guest orelse SubscribeState =:= visitor),
     make_field(<<"visitor">>, bool, SubscribeState =:= visitor)
    ].

vis(true) -> 100;
vis(false) -> 0.

encode_fields(Fields, FromUser) ->
    lists:foldl(fun old_encode_field/2, [], Fields) ++
    lists:foldl(encode_field(_, FromUser, _), [], Fields).

to_field(location, #{coordinates := {Lon, Lat}}, Acc) ->
    [#field{name = <<"location">>, type = geoloc, value = {Lat, Lon}} | Acc];
to_field(user_id, UserID, Acc) ->
    JID = ?wocky_user:to_jid(?wocky_repo:get(?wocky_user, UserID)),
    [#field{name = <<"owner">>, type = jid, value = JID} | Acc];
to_field(public, Public, Acc) ->
    [#field{name = <<"visibility">>, type = int, value = vis(Public)} | Acc];
to_field(updated_at, Updated, Acc) ->
    [#field{name = <<"updated">>, type = timestamp, value = Updated} | Acc];
to_field(subscribers_hash, Hash, Acc) ->
    [#field{name = <<"subscribers+hash">>, type = string, value = Hash} | Acc];
% For ugly reasons we need to subtract one from the count here because the
% owner is always a subscriber, but we don't want to include them in the count:
to_field(subscribers_count, Count, Acc) ->
    [#field{name = <<"subscribers+size">>, type = int,
            value = wocky_bot_subscription:adjust_exclude_owner(Count)}
     | Acc];
to_field(guests_hash, Hash, Acc) ->
    [#field{name = <<"guests+hash">>, type = string, value = Hash} | Acc];
to_field(guests_count, Count, Acc) ->
    [#field{name = <<"guests+size">>, type = int, value = Count} | Acc];
to_field(visitors_hash, Hash, Acc) ->
    [#field{name = <<"visitors+hash">>, type = string, value = Hash} | Acc];
to_field(visitors_count, Count, Acc) ->
    [#field{name = <<"visitors+size">>, type = int, value = Count} | Acc];
to_field(_, null, Acc) -> Acc;
to_field(Key, Val, Acc) ->
    KeyBin = atom_to_binary(Key, utf8),
    case lists:keyfind(KeyBin, #field.name, output_fields()) of
        false -> Acc;
        #field{type = Type} -> [make_field(KeyBin, Type, Val) | Acc]
    end.

make_field(Name, Types, Val)
  when is_list(Types) ->
    make_field(Name, lists:last(Types), Val);
make_field(Name, Type, Val)
  when Type =:= string orelse
       Type =:= int orelse
       Type =:= float orelse
       Type =:= bool orelse
       Type =:= timestamp orelse
       Type =:= tags ->
    #field{name = Name, type = Type, value = Val};
make_field(Name, jid, Val) when is_binary(Val) ->
    #field{name = Name, type = jid, value = safe_jid_from_binary(Val)};
make_field(Name, jid, Val = #jid{}) ->
    #field{name = Name, type = jid, value = Val}.

%%%===================================================================
%%% Field encoding - deprecated <field ... > style
%%%===================================================================

old_encode_field(#field{name = N, type = string, value = V}, Acc) ->
    [field_element(N, string, safe_string(V)) | Acc];
old_encode_field(#field{name = N, type = jid, value = V}, Acc) ->
    [field_element(N, jid, safe_jid_to_binary(V)) | Acc];
old_encode_field(#field{name = N, type = int, value = V}, Acc) ->
    [field_element(N, int, integer_to_binary(V)) | Acc];
old_encode_field(#field{name = N, type = float, value = V}, Acc)
  when is_float(V)->
    [field_element(N, float, float_to_binary(V)) | Acc];
old_encode_field(#field{name = N, type = float, value = V}, Acc)
  when is_integer(V) ->
    [field_element(N, float, float_to_binary(float(V))) | Acc];
old_encode_field(#field{name = N, type = bool, value = V}, Acc) ->
    [field_element(N, bool, atom_to_binary(V, utf8)) | Acc];
old_encode_field(#field{name = N, type = geoloc, value = V}, Acc) ->
    [geoloc_field(N, V) | Acc];
old_encode_field(#field{name = N, type = timestamp, value = V}, Acc) ->
    [field_element(N, timestamp, ?wocky_timestamp:to_string(V)) | Acc];
old_encode_field(#field{name = N, type = tags, value = V}, Acc) ->
    [old_tags_element(N, V) | Acc].

make_jid(User) ->
    case jid:from_binary(User) of
        error -> {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Invalid user">>)};
        JID -> {ok, JID}
    end.

safe_jid_from_binary(<<>>) -> nil;
safe_jid_from_binary(JID) -> jid:from_binary(JID).

safe_jid_to_binary(nil) -> <<>>;
safe_jid_to_binary(JID) -> jid:to_binary(JID).

safe_string(nil) -> <<>>;
safe_string(Str) -> Str.

field_element(Name, Type, Val) ->
    #xmlel{name = <<"field">>,
           attrs = [{<<"var">>, Name},
                    {<<"type">>, atom_to_binary(Type, utf8)}],
           children = [value_element(Val)]}.

value_element(Val) ->
    wocky_xml:cdata_el(<<"value">>, Val).



%%%===================================================================
%%% Field encoding - new <id> style
%%%===================================================================

% TODO: Hack until we remove the old stuff, then we can tidy this up:
encode_field(#field{name = <<"image">>, value = V}, FromUser, Acc) ->
    [wocky_xml:image_element(<<"image">>, V, ?wocky_user:to_jid(FromUser))
     | Acc];
encode_field(#field{name = N, type = string, value = V}, _, Acc) ->
    wocky_util:add_cdata_el(N, safe_string(V), Acc);
encode_field(#field{name = N, type = jid, value = V}, _, Acc) ->
    wocky_util:add_cdata_el(N, safe_jid_to_binary(V), Acc);
encode_field(#field{name = N, type = int, value = V}, _, Acc) ->
    wocky_util:add_cdata_el(N, integer_to_binary(V), Acc);
encode_field(#field{name = N, type = float, value = V}, _, Acc)
  when is_float(V) ->
    wocky_util:add_cdata_el(N, float_to_binary(V), Acc);
encode_field(F = #field{type = float, value = V}, FromUser, Acc)
  when is_integer(V) ->
    encode_field(F#field{value = float(V)}, FromUser, Acc);
encode_field(#field{name = N, type = bool, value = V}, _, Acc) ->
    wocky_util:add_cdata_el(N, atom_to_binary(V, utf8), Acc);
encode_field(#field{name = N, type = geoloc, value = V}, _, Acc) ->
    [#xmlel{name = N, children = [geoloc_element(V)]} | Acc];
encode_field(#field{name = N, type = timestamp, value = V}, _, Acc) ->
    wocky_util:add_cdata_el(N, ?wocky_timestamp:to_string(V), Acc);
encode_field(#field{name = N, type = tags, value = V}, _, Acc) ->
    [tags_element(N, V) | Acc].

geoloc_field(Name, Val) ->
    #xmlel{name = <<"field">>,
           attrs = [{<<"var">>, Name},
                    {<<"type">>, <<"geoloc">>}],
           children = [geoloc_element(Val)]}.

geoloc_element({Lat, Lon}) ->
    #xmlel{name = <<"geoloc">>,
           attrs = [{<<"xmlns">>, ?NS_GEOLOC}],
           children = [wocky_xml:cdata_el(N, wocky_util:coord_to_binary(V))
                       || {N, V} <- [{<<"lat">>, Lat}, {<<"lon">>, Lon}]]}.
old_tags_element(Name, Tags) ->
    #xmlel{name = <<"tags">>,
           attrs = [{<<"var">>, Name},
                    {<<"type">>, <<"tags">>}],
           children = [wocky_xml:cdata_el(<<"tag">>, T) || T <- Tags]}.

tags_element(Name, Tags) ->
    #xmlel{name = Name,
           children = [wocky_xml:cdata_el(<<"tag">>, T) || T <- Tags]}.

make_ret_stanza(Fields) ->
    #xmlel{name = <<"bot">>,
           attrs = [{<<"xmlns">>, ?NS_BOT}],
           children = Fields}.

make_new_id_stanza(NewID) ->
    #xmlel{name = <<"new-id">>,
           attrs = [{<<"xmlns">>, ?NS_BOT}],
           children = [#xmlcdata{content = NewID}]}.

make_guests(Guests) -> make_user_list(<<"guest">>, Guests).
make_visitors(Visitors) -> make_user_list(<<"visitor">>, Visitors).

make_user_list(Name, Users) ->
    [#xmlel{name = Name,
            attrs = [{<<"jid">>, jid:to_binary(
                                   jid:make(
                                     maps:get(id, U),
                                     wocky_xmpp_app:server(), <<>>))}]}
     || U <- Users].

users_result(Name, UserEls, Size, Hash, RSMOut) ->
    #xmlel{name = Name,
           attrs = [{<<"xmlns">>, ?NS_BOT},
                    {<<"size">>, integer_to_binary(Size)},
                    {<<"hash">>, Hash}],
           children = UserEls ++ jlib:rsm_encode(RSMOut)}.

reply_not_allowed(Sender, Stanza) ->
    Err = jlib:make_error_reply(
            Stanza,
            ?ERRT_NOT_ALLOWED(
               ?MYLANG, <<"Bot action not allowed to this user">>)),
    ejabberd_router:route(#jid{lserver = wocky_xmpp_app:server()}, Sender, Err).
