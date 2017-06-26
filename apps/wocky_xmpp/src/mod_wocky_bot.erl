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
-export([start/2, stop/1]).

%% IQ handler
-export([handle_iq/3]).

%% Access manager callback
-export([check_access/3]).

%% Other functions
-export([make_bot_el/2]).

-type loc() :: {float(), float()}.
-type tags() :: [binary()].

-type field_type() :: string | int | bool | geoloc | jid | timestamp | tags.
-type value_type() :: nil | binary() | integer() | boolean()
                      | loc() | jid() | tags() | ?datetime:t().

-record(field, {
          name :: binary(),
          type :: field_type(),
          value :: value_type()
         }).

-define(PACKET_FILTER_PRIORITY, 40).


%%%===================================================================
%%% gen_mod handlers
%%%===================================================================

start(Host, _Opts) ->
    wocky_bot_subscription:start(Host),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_BOT,
                                  ?MODULE, handle_iq, parallel),
    mod_disco:register_feature(Host, ?NS_BOT),
    ejabberd_hooks:add(filter_local_packet, Host,
                       fun filter_local_packet_hook/1,
                       ?PACKET_FILTER_PRIORITY),
    mod_wocky_access:register(<<"bot">>, ?MODULE).

stop(Host) ->
    mod_disco:unregister_feature(Host, ?NS_BOT),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_BOT),
    ejabberd_hooks:delete(filter_local_packet, Host,
                          fun filter_local_packet_hook/1,
                          ?PACKET_FILTER_PRIORITY),
    mod_wocky_access:unregister(<<"bot">>, ?MODULE),
    wocky_bot_subscription:stop(Host).

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
handle_iq_type(From, To, set, <<"delete">>, Attrs, IQ) ->
    handle_owner_action(delete, From, To, Attrs, IQ);

% Retrieve owned bots
handle_iq_type(From, To, get, Name, Attrs, IQ)
  %% We want 'bot' for retrieving a single bot and 'bots' for a list of bots.
  %% The documentation is inconsistent, so to avoid breaking anything, we will
  %% accept either.
  when Name =:= <<"bot">> orelse Name =:= <<"bots">> ->
    case wocky_bot_util:get_id_from_node(Attrs) of
        {ok, _ID} -> handle_access_action(get_bot, From, To, Attrs, IQ);
        {error, _} ->
            case get_location_from_attrs(Attrs) of
                {ok, {Lat, Lon}} ->
                    get_bots_near_location(From, Lat, Lon);

                {error, _} ->
                    get_bots_for_user(From, IQ, Attrs)
            end
    end;

% Retrieve subscribed bots
handle_iq_type(From, To, get, Name, _Attrs, IQ)
  when Name =:= <<"following">> orelse %% Backwards compatability only
       Name =:= <<"subscribed">> ->
    handle_subscribed(From, To, IQ);

% Update
handle_iq_type(From, To, set, <<"fields">>, Attrs, IQ) ->
    handle_owner_action(update, From, To, Attrs, IQ);

% Subscribe
handle_iq_type(From, To, set, <<"subscribe">>, Attrs, IQ) ->
    handle_access_action(subscribe, From, To, Attrs, IQ);

% Unsubscribe
handle_iq_type(From, To, set, <<"unsubscribe">>, Attrs, _IQ) ->
    handle_unsubscribe(From, To, Attrs);

% Retrieve subscribers
handle_iq_type(From, To, get, <<"subscribers">>, Attrs, IQ) ->
    handle_owner_action(subscribers, From, To, Attrs, IQ);

% Retrieve item(s)
handle_iq_type(From, To, get, <<"query">>, Attrs, IQ) ->
    handle_access_action(item_query, From, To, Attrs, IQ);

% Publish an item
handle_iq_type(From, To, set, <<"publish">>, Attrs, IQ) ->
    handle_owner_action(publish, From, To, Attrs, IQ);

% Delete an item
handle_iq_type(From, To, set, <<"retract">>, Attrs, IQ) ->
    handle_owner_action(retract, From, To, Attrs, IQ);

% Get a list of images from items on the bot
handle_iq_type(From, To, get, <<"item_images">>, Attrs, IQ) ->
    handle_access_action(item_images, From, To, Attrs, IQ);

% Follow me
handle_iq_type(From, To, set, <<"follow-me">>, Attrs, IQ) ->
    handle_owner_action(follow_me, From, To, Attrs, IQ);

% Un-follow me
handle_iq_type(From, To, set, <<"un-follow-me">>, Attrs, IQ) ->
    handle_owner_action(unfollow_me, From, To, Attrs, IQ);

handle_iq_type(_From, _To, _Type, _Op, _Attrs, _IQ) ->
    {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Invalid query">>)}.


%%%===================================================================
%%% Actions that only the owner may perform
%%%===================================================================

handle_owner_action(Action, From, To, Attrs, IQ) ->
    do([error_m ||
           Bot <- wocky_bot_util:get_bot_from_node(Attrs),
           wocky_bot_util:check_owner(Bot, From),
           perform_owner_action(Action, Bot, From, To, IQ)
       ]).

perform_owner_action(update, Bot, _From, #jid{lserver = Server}, IQ) ->
    #iq{sub_el = #xmlel{children = Children}} = IQ,
    do([error_m ||
        Fields <- get_fields(Children),
        OldPublic = ?wocky_bot:'public?'(Bot),
        FieldsMap = normalise_fields(Fields),
        NewBot <- ?wocky_bot:update(Bot, FieldsMap),
        wocky_bot_users:notify_new_viewers(Server, NewBot, OldPublic,
                                           ?wocky_bot:'public?'(NewBot)),
        wocky_bot_users:maybe_notify_subscribers(Server, Bot, NewBot),
        {ok, []}
       ]);

perform_owner_action(delete, Bot, _From, _To, _IQ) ->
    ?wocky_bot:delete(Bot),
    {ok, []};

perform_owner_action(subscribers, Bot, _From, _To, _IQ) ->
    wocky_bot_subscription:retrieve_subscribers(Bot);

perform_owner_action(publish, Bot, _From, To, #iq{sub_el = SubEl}) ->
    wocky_bot_item:publish(Bot, To, SubEl);

perform_owner_action(retract, Bot, _From, To, #iq{sub_el = SubEl}) ->
    wocky_bot_item:retract(Bot, To, SubEl);

perform_owner_action(follow_me, Bot, From, _To, IQ) ->
    #iq{sub_el = #xmlel{attrs = Attrs}} = IQ,
    do([error_m ||
        Expiry <- get_follow_me_expiry(Attrs),
        ?wocky_bot:update(Bot, #{follow_me => true,
                                 follow_me_expiry => Expiry}),
        publish_follow_me(From, Bot),
        wocky_bot_expiry_mon:follow_started(?wocky_bot:to_jid(Bot), Expiry),
        {ok, follow_me_result(IQ)}
       ]);

perform_owner_action(unfollow_me, Bot, From, _To, IQ) ->
    do([error_m ||
        ?wocky_bot:update(Bot, #{follow_me => false, follow_me_expiry => nil}),
        publish_unfollow_me(From, Bot),
        wocky_bot_expiry_mon:follow_stopped(?wocky_bot:to_jid(Bot)),
        {ok, follow_me_result(IQ)}
       ]).

get_follow_me_expiry(Attrs) ->
    case wocky_xml:get_attr(<<"expiry">>, Attrs) of
        {error, _} = E -> E;
        {ok, Expiry} -> ?wocky_timestamp:from_string(Expiry)
    end.

follow_me_result(#iq{sub_el = SubEl}) ->
    SubEl.

publish_follow_me(Owner, Bot) ->
    Stanza = wocky_bot_util:follow_stanza(Bot, <<"follow on">>),
    send_hs_notification(Owner, Bot, Stanza).

publish_unfollow_me(Owner, Bot) ->
    Stanza = wocky_bot_util:follow_stanza(Bot, <<"follow off">>),
    send_hs_notification(Owner, Bot, Stanza).

send_hs_notification(From, Bot, Stanza) ->
    ejabberd_router:route(?wocky_bot:to_jid(Bot), From, Stanza).


%%%===================================================================
%%% Actions that require the user to have access to the bot
%%%===================================================================

handle_access_action(Action, From, _To, Attrs, IQ) ->
    do([error_m ||
           Bot <- wocky_bot_util:get_bot_from_node(Attrs),
           User <- wocky_bot_util:get_user_from_jid(From),
           wocky_bot_util:check_access(User, Bot),
           perform_access_action(Action, Bot, User, IQ)
       ]).

perform_access_action(get_bot, Bot, User, _IQ) ->
    do([error_m ||
           BotEl <- make_bot_el(Bot, User),
           {ok, BotEl}
       ]);

perform_access_action(item_query, Bot, _User, IQ) ->
    wocky_bot_item:query(Bot, IQ);

perform_access_action(item_images, Bot, _User, IQ) ->
    wocky_bot_item:query_images(Bot, IQ);

perform_access_action(subscribe, Bot, User, _IQ) ->
    wocky_bot_subscription:subscribe(User, Bot).

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
        {ID, PendingBot} <- get_id_and_bot(Fields),
        User <- wocky_bot_util:get_user_from_jid(From),
        Fields2 <- add_server(Fields, Server),
        Fields3 <- add_defaults(Fields2),
        check_required_fields(Fields3, required_fields()),
        FieldsMap = normalise_fields(Fields3),
        Bot <- create_bot(ID, PendingBot, User, FieldsMap),
        BotEl <- make_bot_el(Bot, User),
        wocky_bot_users:notify_new_viewers(Server, Bot, none,
                                           ?wocky_bot:'public?'(Bot)),
        {ok, BotEl}
       ]).

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

get_id_and_bot(Fields) ->
    case lists:keyfind(<<"id">>, #field.name, Fields) of
        false -> {ok, {?wocky_id:new(), nil}};
        #field{value = ID} -> check_id(ID)
    end.

check_id(ID) ->
    case ?wocky_repo:get(?wocky_bot, ID) of
        nil -> {error, ?ERR_ITEM_NOT_FOUND};
        Bot -> {ok, {ID, Bot}}
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
        {SubscribedBots, RSMOut} <- get_subscribed_bots(User, RSMIn),
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

get_bots_near_location(From, Lat, Lon) ->
    case 'Elixir.Wocky.Index':geosearch(Lat, Lon) of
        {ok, AllBots} ->
            User = ?wocky_user:get_by_jid(From),
            VisibleBots = lists:filter(
                            ?wocky_user:'searchable?'(User, _), AllBots),
            {ok, make_geosearch_result(VisibleBots)};
        {error, indexing_disabled} ->
            {error,
             ?ERRT_FEATURE_NOT_IMPLEMENTED(
                ?MYLANG, <<"Index search is not configured on this server">>)}
    end.

get_subscribed_bots(User, RSMIn) ->
    BaseQuery = ?wocky_user:subscribed_bots_query(User),
    FilteredQuery = ?wocky_bot:read_access_filter(BaseQuery, User),
    {ok,
     ?wocky_rsm_helper:rsm_query(RSMIn, FilteredQuery, id, {asc, updated_at})}.

make_geosearch_result(Bots) ->
    #xmlel{name = <<"bots">>,
           attrs = [{<<"xmlns">>, ?NS_BOT}],
           children = make_geosearch_els(Bots)}.

make_geosearch_els(Bots) ->
    [make_geosearch_el(Bot) || Bot <- Bots].

geosearch_el_fields() ->
    [id, server, title, image, lat, lon, radius, distance].

make_geosearch_el(Bot) ->
    JidField = make_field(<<"jid">>, jid, ?wocky_bot:to_jid(Bot)),
    MapFields = map_to_fields(maps:with(geosearch_el_fields(), Bot)),
    RetFields = encode_fields([JidField | MapFields]),
    make_ret_stanza(RetFields).

get_bots_for_user(From, IQ, Attrs) ->
    do([error_m ||
        UserBin <- wocky_xml:get_attr(<<"user">>, Attrs),
        UserJID <- make_jid(UserBin),
        User <- wocky_bot_util:get_user_from_jid(UserJID),
        RSMIn <- rsm_util:get_rsm(IQ),
        FromUser <- wocky_bot_util:get_user_from_jid(From),
        {FilteredBots, RSMOut} <- get_bots_for_user_rsm(User, FromUser, RSMIn),
        {ok, users_bots_result(FilteredBots, User, RSMOut)}
       ]).

get_bots_for_user_rsm(User, QueryingUser, RSMIn) ->
    BaseQuery = ?wocky_user:owned_bots_query(User),
    FilteredQuery = ?wocky_bot:read_access_filter(BaseQuery, QueryingUser),
    {ok,
     ?wocky_rsm_helper:rsm_query(RSMIn, FilteredQuery, id, {asc, updated_at})}.

users_bots_result(Bots, User, RSMOut) ->
    BotEls = make_bot_els(Bots, User),
    #xmlel{name = <<"bots">>,
           attrs = [{<<"xmlns">>, ?NS_BOT}],
           children = BotEls ++ jlib:rsm_encode(RSMOut)}.

make_bot_els(Bots, User) ->
    lists:map(do_make_bot_els(_, User), Bots).

do_make_bot_els(Bot, User) ->
    {ok, El} = make_bot_el(Bot, User),
    El.


%%%===================================================================
%%% Incoming packet handler
%%%===================================================================

-type filter_packet() :: {ejabberd:jid(), ejabberd:jid(), jlib:xmlel()}.
-spec filter_local_packet_hook(filter_packet() | drop) ->
    filter_packet() | drop.

%% Packets to the bot - dropped if they were processed here.
filter_local_packet_hook(P = {From,
                              #jid{user = <<>>, lserver = LServer,
                                   resource= <<"bot/", BotID/binary>>},
                              Packet}) ->
    case handle_bot_packet(From, LServer, BotID, Packet) of
        ok -> drop;
        ignored -> P
    end;

%% Packets to another entity which reference a bot. Passed through unless
%% they hit a condition to block them.
filter_local_packet_hook(P = {From, To,
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

% Presence packets are handled in the user_send_packet hook in
% wocky_bot_subscriber however that hook can't drop them (and as a result
% they return errors back to the sender). So we cause them to be dropped here.
handle_bot_packet(_From, _LServer, _BotID,
                  #xmlel{name = <<"presence">>}) ->
    ok;
handle_bot_packet(_, _, _, _) ->
    ignored.

handle_headline_packet(From, To, Stanza) ->
    case xml:get_subtag(Stanza, <<"bot">>) of
        false -> ok;
        BotStanza -> handle_bot_stanza(From, To, BotStanza)
    end.

handle_bot_stanza(From, To, BotStanza) ->
    case wocky_bot_util:bot_packet_action(BotStanza) of
        {JIDBin, share} ->
            wocky_bot_users:handle_share(From, To, jid:from_binary(JIDBin));
        _ ->
            ok
    end.

%%%===================================================================
%%% Access manager callback
%%%===================================================================

-spec check_access(binary(), ejabberd:jid(), mod_wocky_access:op()) ->
    mod_wocky_access:access_result().
check_access(BotNode, ActorJID, view) ->
    BotID = ?wocky_bot:get_id_from_node(BotNode),
    R = do([error_m ||
               Bot <- wocky_bot_util:get_bot(BotID),
               Actor <- wocky_bot_util:get_user_from_jid(ActorJID),
               {ok, ?wocky_user:'can_access?'(Actor, Bot)}
           ]),
    allow_to_result(R);

check_access(BotNode, ActorJID, _) ->
    BotID = ?wocky_bot:get_id_from_node(BotNode),
    R = do([error_m ||
               Bot <- wocky_bot_util:get_bot(BotID),
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
get_fields([El = #xmlel{name = <<"field">>,
                        attrs = Attrs}
            | Rest] , Acc) ->
    F = do([error_m ||
            Name <- wocky_xml:get_attr(<<"var">>, Attrs),
            TypeBin <- wocky_xml:get_attr(<<"type">>, Attrs),
            Type <- check_field(Name, TypeBin),
            Value <- get_field_value(Type, El),
            #field{name = Name, type = Type, value = Value}
           ]),
    get_fields(Rest, [F | Acc]).

get_field_value(string, FieldEl) ->
    wocky_xml:get_subel_cdata(<<"value">>, FieldEl);

get_field_value(int, FieldEl) ->
    wocky_xml:act_on_subel_cdata(<<"value">>, FieldEl, fun read_integer/1);

get_field_value(geoloc, FieldEl) ->
    wocky_xml:act_on_subel(<<"geoloc">>, FieldEl, fun read_geoloc/1);

get_field_value(tags, FieldEl) ->
    wocky_xml:foldl_subels(FieldEl, [], fun read_tag/2);

get_field_value(bool, FieldEl) ->
    wocky_xml:act_on_subel_cdata(<<"value">>, FieldEl, fun read_bool/1).

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

check_required_fields(_Fields, []) -> ok;
check_required_fields(Fields, [#field{name = Name} | Rest]) ->
    case lists:any(fun(#field{name = FName}) -> FName =:= Name end, Fields) of
        true ->
            check_required_fields(Fields, Rest);
        false ->
            {error, ?ERRT_BAD_REQUEST(?MYLANG,
                                      <<"Missing ", Name/binary, " field">>)}
    end.

check_field(Name, TypeBin) ->
    case lists:keyfind(Name, #field.name, create_fields()) of
        #field{type = ExpectedType} ->
            check_field_type(Name, TypeBin, ExpectedType);
        false ->
            {error, ?ERRT_BAD_REQUEST(
                       ?MYLANG, <<"Invalid field ", Name/binary>>)}
    end.

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
     field(<<"radius">>,        int,    0)].

optional_fields() ->
    [field(<<"id">>,            string, <<>>),
     field(<<"description">>,   string, <<>>),
     field(<<"shortname">>,     string, <<>>),
     field(<<"image">>,         string, <<>>),
     field(<<"type">>,          string, <<>>),
     field(<<"address">>,       string, <<>>),
     field(<<"visibility">>,    int,    ?WOCKY_BOT_VIS_OWNER),
     field(<<"public">>,        bool,   false),
     field(<<"alerts">>,        int,    0),
     field(<<"tags">>,          tags,   [])].

output_only_fields() ->
    [field(<<"server">>,        string, <<>>),
     field(<<"owner">>,         jid,    <<>>),
     field(<<"updated">>,       timestamp, <<>>),
     field(<<"subscribed">>,    bool,   false),
     field(<<"distance">>,      int,    0)].

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
    Acc#{lat => Lat, lon => Lon};
normalise_field(#field{type = jid, value = #jid{luser = UserID}}, Acc) ->
    Acc#{user_id => UserID};
normalise_field(#field{name = <<"visibility">>, value = 100}, Acc) ->
    Acc#{public => true};
normalise_field(#field{name = <<"visibility">>}, Acc) ->
    Acc#{public => false};
normalise_field(#field{name = <<"alerts">>, value = 1}, Acc) ->
    Acc#{alerts => true};
normalise_field(#field{name = <<"alerts">>, value = 0}, Acc) ->
    Acc#{alerts => false};
normalise_field(#field{name = N, value = V}, Acc) ->
    Acc#{binary_to_existing_atom(N, utf8) => V}.

make_bot_el(Bot, User) ->
    RetFields = make_ret_elements(Bot, User),
    {ok, make_ret_stanza(RetFields)}.

make_ret_elements(Bot, User) ->
    MetaFields = meta_fields(Bot, User),
    Fields = map_to_fields(Bot),
    encode_fields(Fields ++ MetaFields).

meta_fields(Bot, User) ->
    Subscribers = ?wocky_bot:subscribers(Bot),
    ImageItems = ?wocky_item:get_image_count(Bot),
    Subscribed = ?wocky_user:'subscribed?'(User, Bot),
    [make_field(<<"jid">>, jid, ?wocky_bot:to_jid(Bot)),
     make_field(<<"image_items">>, int, ImageItems),
     make_field(<<"subscribed">>, bool, Subscribed)
     | size_and_hash(<<"subscribers">>, Subscribers)].

size_and_hash(Name, List) ->
    [make_field(<<Name/binary, "+size">>, int, length(List)),
     make_field(<<Name/binary, "+hash">>, string,
                wocky_bot_util:list_hash(List))
    ].

map_to_fields(Map = #{lat := Lat, lon := Lon}) ->
    [#field{name = <<"location">>, type = geoloc, value = {Lat, Lon}} |
     map_to_fields(maps:without([lat, lon], Map))];
map_to_fields(Map = #{user_id := UserID}) ->
    User = ?wocky_repo:get(?wocky_user, UserID),
    [#field{name = <<"owner">>, type = jid, value = ?wocky_user:to_jid(User)} |
     map_to_fields(maps:without([user_id], Map))];
map_to_fields(Map = #{public := Public}) ->
    [#field{name = <<"visibility">>, type = int, value = vis(Public)} |
     map_to_fields(maps:without([public], Map))];
map_to_fields(Map = #{alerts := Alerts}) ->
    [#field{name = <<"alerts">>, type = int, value = alerts(Alerts)} |
     map_to_fields(maps:without([alerts], Map))];
map_to_fields(Map = #{updated_at := Updated}) ->
     [#field{name = <<"updated">>, type = timestamp, value = Updated} |
     map_to_fields(maps:without([updated_at], Map))];
map_to_fields(Map) ->
    maps:fold(fun to_field/3, [], Map).

vis(true) -> 100;
vis(false) -> 0.

alerts(true) -> 1;
alerts(false) -> 0.

encode_fields(Fields) ->
    lists:foldl(fun encode_field/2, [], Fields).

to_field(_, null, Acc) -> Acc;
to_field(Key, Val, Acc) ->
    KeyBin = atom_to_binary(Key, utf8),
    case lists:keyfind(KeyBin, #field.name, output_fields()) of
        false -> Acc;
        #field{type = Type} -> [make_field(KeyBin, Type, Val) | Acc]
    end.

make_field(Name, Type, Val)
  when Type =:= string orelse
       Type =:= int orelse
       Type =:= bool orelse
       Type =:= timestamp orelse
       Type =:= tags ->
    #field{name = Name, type = Type, value = Val};
make_field(Name, jid, Val) when is_binary(Val) ->
    #field{name = Name, type = jid, value = safe_jid_from_binary(Val)};
make_field(Name, jid, Val = #jid{}) ->
    #field{name = Name, type = jid, value = Val}.

encode_field(#field{name = N, type = string, value = V}, Acc) ->
    [field_element(N, string, safe_string(V)) | Acc];
encode_field(#field{name = N, type = jid, value = V}, Acc) ->
    [field_element(N, jid, safe_jid_to_binary(V)) | Acc];
encode_field(#field{name = N, type = int, value = V}, Acc) ->
    [field_element(N, int, integer_to_binary(V)) | Acc];
encode_field(#field{name = N, type = bool, value = V}, Acc) ->
    [field_element(N, bool, atom_to_binary(V, utf8)) | Acc];
encode_field(#field{name = N, type = geoloc, value = V}, Acc) ->
    [geoloc_field(N, V) | Acc];
encode_field(#field{name = N, type = timestamp, value = V}, Acc) ->
    [field_element(N, timestamp, ?wocky_timestamp:to_string(V)) | Acc];
encode_field(#field{name = N, type = tags, value = V}, Acc) ->
    [tags_element(N, V) | Acc].

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
tags_element(Name, Tags) ->
    #xmlel{name = <<"tags">>,
           attrs = [{<<"var">>, Name},
                    {<<"type">>, <<"tags">>}],
           children = [wocky_xml:cdata_el(<<"tag">>, T) || T <- Tags]}.

make_ret_stanza(Fields) ->
    #xmlel{name = <<"bot">>,
           attrs = [{<<"xmlns">>, ?NS_BOT}],
           children = Fields}.

make_new_id_stanza(NewID) ->
    #xmlel{name = <<"new-id">>,
           attrs = [{<<"xmlns">>, ?NS_BOT}],
           children = [#xmlcdata{content = NewID}]}.

reply_not_allowed(Sender, Stanza) ->
    Err = jlib:make_error_reply(
            Stanza,
            ?ERRT_NOT_ALLOWED(
               ?MYLANG, <<"Bot action not allowed to this user">>)),
    ejabberd_router:route(#jid{lserver = wocky_xmpp_app:server()}, Sender, Err).
