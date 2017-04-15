%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module implementing Wocky bots
%%% See https://github.com/hippware/tr-wiki/wiki/Bot
%%%
-module(mod_wocky_bot).

-behaviour(gen_mod).
-behaviour(wocky_access_manager).

-compile({parse_transform, do}).
-compile({parse_transform, cut}).
-compile({parse_transform, fun_chain}).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").
-include("wocky.hrl").
-include("wocky_bot.hrl").
-include("wocky_roster.hrl").

%% gen_mod handlers
-export([start/2, stop/1]).

%% IQ handler
-export([handle_iq/3]).

%% Access manager callback
-export([check_access/3]).

-type loc() :: {float(), float()}.
-type tags() :: [binary()].

-type field_type() :: string | int | bool | geoloc | jid | timestamp | tags.
-type value_type() :: not_found | binary() | integer() | boolean()
                      | loc() | jid() | tags().


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
    case handle_iq_type(From, To, IQ) of
        {ok, SubEl} -> IQ#iq{type = result, sub_el = SubEl};
        {error, Error} -> wocky_util:make_error_iq_response(IQ, Error)
    end.

% New ID
handle_iq_type(From, _To, #iq{type = set,
                              sub_el = #xmlel{name = <<"new-id">>}}) ->
    handle_new_id(From);

% Create
handle_iq_type(From, _To, #iq{type = set,
                              sub_el = #xmlel{name = <<"create">>,
                                              children = Children}
                             }) ->
    handle_create(From, Children);

% Delete
handle_iq_type(From, To, #iq{type = set,
                             sub_el = #xmlel{name = <<"delete">>,
                                             attrs = Attrs}
                            }) ->
    handle_delete(From, To, Attrs);

% Retrieve owned bots
handle_iq_type(From, To, IQ = #iq{type = get,
                                  sub_el = #xmlel{name = Name,
                                                  attrs = Attrs}
                                 })
  %% We want 'bot' for retrieving a single bot and 'bots' for a list of bots.
  %% The documentation is inconsistent, so to avoid breaking anything, we will
  %% accept either.
  when Name =:= <<"bot">> orelse Name =:= <<"bots">> ->
    handle_get(From, To, IQ, Attrs);

% Retrieve subscribed bots
handle_iq_type(From, To, IQ = #iq{type = get,
                                  sub_el = #xmlel{name = Name}
                                 })
  when Name =:= <<"following">> orelse %% Backwards compatability only
       Name =:= <<"subscribed">> ->
    handle_subscribed(From, To, IQ);

% Update
handle_iq_type(From, To, #iq{type = set,
                             sub_el = #xmlel{name = <<"fields">>,
                                             attrs = Attrs,
                                             children = Children}
                            }) ->
    handle_update(From, To, Attrs, Children);

% Subscribe
handle_iq_type(From, To, #iq{type = set,
                             sub_el = #xmlel{name = <<"subscribe">>,
                                             attrs = Attrs}
                            }) ->
    wocky_bot_subscription:handle_subscribe(From, To, Attrs);

% Unsubscribe
handle_iq_type(From, To, #iq{type = set,
                             sub_el = #xmlel{name = <<"unsubscribe">>,
                                             attrs = Attrs}
                            }) ->
    wocky_bot_subscription:handle_unsubscribe(From, To, Attrs);

% Retrieve subscribers
handle_iq_type(From, To, #iq{type = get,
                             sub_el = #xmlel{name = <<"subscribers">>,
                                             attrs = Attrs}
                            }) ->
    wocky_bot_subscription:handle_retrieve_subscribers(From, To, Attrs);

% Publish an item
handle_iq_type(From, To, #iq{type = set,
                             sub_el = SubEl = #xmlel{name = <<"publish">>,
                                                     attrs = Attrs}
                            }) ->
    wocky_bot_item:handle_publish(From, To, SubEl, Attrs);

% Retrieve item(s)
handle_iq_type(From, To, IQ = #iq{type = get,
                                  sub_el = #xmlel{name = <<"query">>,
                                                  attrs = Attrs}
                                 }) ->
    wocky_bot_item:handle_query(From, To, IQ, Attrs);

% Delete an item
handle_iq_type(From, To, #iq{type = set,
                             sub_el = SubEl = #xmlel{name = <<"retract">>,
                                                     attrs = Attrs}
                            }) ->
    wocky_bot_item:handle_retract(From, To, SubEl, Attrs);

% Get a list of images from items on the bot
handle_iq_type(From, To, IQ = #iq{type = get,
                                  sub_el = #xmlel{name = <<"item_images">>,
                                                  attrs = Attrs}}) ->
    handle_item_images(From, To, IQ, Attrs);

% Follow me
handle_iq_type(From, To, IQ = #iq{type = set,
                                  sub_el = #xmlel{name = <<"follow-me">>,
                                                  attrs = Attrs}}) ->
    handle_follow_me(From, To, IQ, Attrs);

% Un-follow me
handle_iq_type(From, To, IQ = #iq{type = set,
                                  sub_el = #xmlel{name = <<"un-follow-me">>,
                                                  attrs = Attrs}}) ->
    handle_unfollow_me(From, To, IQ, Attrs);

handle_iq_type(_From, _To, _IQ) ->
    {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Invalid query">>)}.

%%%===================================================================
%%% Action - new-id
%%%===================================================================

handle_new_id(From) ->
    ID = wocky_db_bot:new_id(From),
    {ok, make_new_id_stanza(ID)}.

%%%===================================================================
%%% Action - create
%%%===================================================================

handle_create(From, Children) ->
    Server = wocky_xmpp_app:server(),
    do([error_m ||
        Fields <- get_fields(Children),
        Fields2 <- add_server(Fields, Server),
        check_required_fields(Fields2, required_fields()),
        {ID, BotEl} <- create(From, Server, Fields2),
        wocky_bot_users:notify_new_viewers(Server, ID, none,
                                           wocky_db_bot:is_public(Server, ID)),
        {ok, BotEl}
       ]).

add_server(Fields, Server) ->
    {ok,
     [#field{name = <<"server">>, type = string, value = Server} | Fields]}.

%%%===================================================================
%%% Action - delete
%%%===================================================================

handle_delete(From, #jid{lserver = Server}, Attrs) ->
    do([error_m ||
        ID <- wocky_bot_util:get_id_from_node(Attrs),
        wocky_bot_util:check_owner(Server, ID, From),
        wocky_db_bot:delete(Server, ID),
        {ok, []}
       ]).

%%%===================================================================
%%% Actions - get/subscribed
%%%===================================================================

handle_get(From, #jid{lserver = Server}, IQ, Attrs) ->
    case wocky_bot_util:get_id_from_node(Attrs) of
        {ok, ID} -> get_bot_by_id(From, Server, ID);
        {error, _} ->
            case get_location_from_attrs(Attrs) of
                {ok, {Lat, Lon}} ->
                    get_bots_near_location(From, Server, IQ, Lat, Lon);

                {error, _} ->
                    get_bots_for_user(From, Server, IQ, Attrs)
            end
    end.

handle_subscribed(From, #jid{lserver = Server}, IQ) ->
    do([error_m ||
        RSMIn <- rsm_util:get_rsm(IQ),
        BotJIDs <- {ok, wocky_db_bot:subscribed_bots(From)},
        {Bots, RSMOut} <- filter_bots_for_user(BotJIDs, Server, From, RSMIn),
        {ok, users_bots_result(Bots, From, RSMOut)}
       ]).

get_bot_by_id(From, Server, ID) ->
    do([error_m ||
        wocky_bot_util:check_access(Server, ID, From),
        BotEl <- make_bot_el(Server, ID, From),
        {ok, BotEl}
       ]).

get_location_from_attrs(Attrs) ->
    do([error_m ||
        Lat <- wocky_xml:get_attr(<<"lat">>, Attrs),
        Lon <- wocky_xml:get_attr(<<"lon">>, Attrs),
        {ok, {Lat, Lon}}
       ]).

get_bots_near_location(From, _Server, _IQ, Lat, Lon) ->
    case 'Elixir.Wocky.Index':geosearch(Lat, Lon) of
        {ok, AllBots} ->
            VisibleBots = lists:filter(
                            geosearch_access_filter(From, _), AllBots),
            {ok, make_geosearch_result(VisibleBots)};
        {error, no_index_configured} ->
            {error,
             ?ERRT_FEATURE_NOT_IMPLEMENTED(
                ?MYLANG, <<"Index search is not configured on this server">>)}
    end.

geosearch_access_filter(From, #{server := Server, id := ID}) ->
    ok =:= wocky_bot_util:check_access(Server, ID, From).

make_geosearch_result(Bots) ->
    #xmlel{name = <<"bots">>,
           attrs = [{<<"xmlns">>, ?NS_BOT}],
           children = make_geosearch_els(Bots)}.

make_geosearch_els(Bots) ->
    [make_geosearch_el(Bot) || Bot <- Bots].

geosearch_el_fields() ->
    [id, server, title, image, lat, lon, radius, distance].

make_geosearch_el(#{server :=  Server, id := ID} = Bot) ->
    JidField = make_field(<<"jid">>, jid, bot_jid(Server, ID)),
    MapFields = map_to_fields(maps:with(geosearch_el_fields(), Bot)),
    RetFields = encode_fields([JidField | MapFields]),
    make_ret_stanza(RetFields).

get_bots_for_user(From, Server, IQ, Attrs) ->
    do([error_m ||
        User <- wocky_xml:get_attr(<<"user">>, Attrs),
        UserJID <- make_jid(User),
        RSMIn <- rsm_util:get_rsm(IQ),
        BotJIDs <- {ok, wocky_db_bot:owned_bots(UserJID)},
        {Bots, RSMOut} <- filter_bots_for_user(BotJIDs, Server, From, RSMIn),
        {ok, users_bots_result(Bots, From, RSMOut)}
       ]).

filter_bots_for_user(BotJIDs, Server, From, RSMIn) ->
    VisibleJIDs = lists:filter(access_filter(Server, From, _), BotJIDs),
    % Sort bots with most recently updated last
    Bots = [wocky_db_bot:get_bot(JID) || JID <- VisibleJIDs],
    SortedBots = lists:sort(update_order(_, _), Bots),
    {FilteredBots, RSMOut} = rsm_util:filter_with_rsm(SortedBots, RSMIn),
    {ok, {FilteredBots, RSMOut}}.

update_order(#{updated := U1}, #{updated := U2}) ->
    U1 =< U2.

access_filter(Server, From, JID) ->
    ID = wocky_bot_util:get_id_from_jid(JID),
    ok =:= wocky_bot_util:check_access(Server, ID, From).

users_bots_result(Bots, UserJID, RSMOut) ->
    BotEls = make_bot_els(Bots, UserJID),
    #xmlel{name = <<"bots">>,
           attrs = [{<<"xmlns">>, ?NS_BOT}],
           children = BotEls ++ jlib:rsm_encode(RSMOut)}.

make_bot_els(BotIDs, UserJID) ->
    lists:reverse(
      lists:foldl(make_bot_els(_, UserJID, _), [], BotIDs)).

make_bot_els(ID, UserJID, Acc) ->
    {ok, El} = make_bot_el(ID, UserJID),
    [El|Acc].

%%%===================================================================
%%% Action - update
%%%===================================================================

handle_update(From, #jid{lserver = Server}, Attrs, Children) ->
    do([error_m ||
        ID <- wocky_bot_util:get_id_from_node(Attrs),
        wocky_bot_util:check_owner(Server, ID, From),
        OldPublic <- {ok, wocky_db_bot:is_public(Server, ID)},
        Fields <- get_fields(Children),
        update_bot(Server, ID, Fields),
        wocky_bot_users:notify_new_viewers(Server, ID, OldPublic,
                                           wocky_db_bot:is_public(Server, ID)),
        {ok, []}
       ]).

%%%===================================================================
%%% Action - item_images
%%%===================================================================

handle_item_images(From, #jid{lserver = Server}, IQ, Attrs) ->
    do([error_m ||
        ID <- wocky_bot_util:get_id_from_node(Attrs),
        RSMIn <- rsm_util:get_rsm(IQ),
        wocky_bot_util:check_access(Server, ID, From),
        Images <- get_bot_item_images(Server, ID),
        {FilteredImages, RSMOut} <-
        {ok, rsm_util:filter_with_rsm(Images, RSMIn)},
        Owner <- {ok, wocky_db_bot:owner(ID)},
        {ok, images_result(Owner, FilteredImages, RSMOut)}
       ]).

get_bot_item_images(Server, ID) ->
    Images = wocky_db_bot:item_images(Server, ID),
    {ok, lists:sort(update_order(_, _), Images)}.

images_result(Owner, Images, RSMOut) ->
    ImageEls = image_els(Owner, Images),
    #xmlel{name = <<"item_images">>,
           attrs = [{<<"xmlns">>, ?NS_BOT}],
           children = ImageEls ++ jlib:rsm_encode(RSMOut)}.

image_els(Owner, Images) ->
    lists:map(image_el(Owner, _), Images).

image_el(Owner, #{id := ID, image := Image, updated := Updated}) ->
    #xmlel{name = <<"image">>,
           attrs = [{<<"owner">>, jid:to_binary(Owner)},
                    {<<"item">>, ID},
                    {<<"url">>, Image},
                    {<<"updated">>, wocky_db:timestamp_to_string(Updated)}]}.

%%%===================================================================
%%% Action - follow-me
%%%===================================================================

handle_follow_me(From, #jid{lserver = Server}, IQ, Attrs) ->
    do([error_m ||
        ID <- wocky_bot_util:get_id_from_node(Attrs),
        wocky_bot_util:check_owner(Server, ID, From),
        Expiry <- get_follow_me_expiry(Attrs),
        wocky_db_bot:set_follow_me(ID, Expiry),
        publish_follow_me(From, ID, Server),
        wocky_bot_expiry_mon:follow_started(
          wocky_bot_util:make_jid(Server, ID), Expiry),
        {ok, follow_me_result(IQ)}
       ]).

handle_unfollow_me(From, #jid{lserver = Server}, IQ, Attrs) ->
    do([error_m ||
        ID <- wocky_bot_util:get_id_from_node(Attrs),
        wocky_bot_util:check_owner(Server, ID, From),
        wocky_db_bot:set_unfollow_me(ID),
        publish_unfollow_me(From, ID, Server),
        wocky_bot_expiry_mon:follow_stopped(
          wocky_bot_util:make_jid(Server, ID)),
        {ok, follow_me_result(IQ)}
       ]).

get_follow_me_expiry(Attrs) ->
    case wocky_xml:get_attr(<<"expiry">>, Attrs) of
        {error, _} = E -> E;
        {ok, Expiry} -> {ok, binary_to_integer(Expiry)}
    end.

follow_me_result(#iq{sub_el = SubEl}) ->
    SubEl.

publish_follow_me(Owner, ID, Server) ->
    Stanza = wocky_bot_util:follow_stanza(Server, ID, <<"follow on">>),
    send_hs_notification(Owner, ID, Server, Stanza).

publish_unfollow_me(Owner, ID, Server) ->
    Stanza = wocky_bot_util:follow_stanza(Server, ID, <<"follow off">>),
    send_hs_notification(Owner, ID, Server, Stanza).

send_hs_notification(From, ID, Server, Stanza) ->
    ejabberd_router:route(wocky_bot_util:make_jid(Server, ID), From, Stanza).

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
check_access(<<"bot/", ID/binary>>, Actor, view) ->
    allow_to_result(
      wocky_db_bot:has_access(wocky_xmpp_app:server(), ID, Actor));

check_access(<<"bot/", ID/binary>>, Actor, _) ->
    case wocky_db_bot:owner(ID) of
        not_found -> deny;
        Owner -> allow_to_result(jid:are_bare_equal(Owner, Actor))
    end.

allow_to_result(true) -> allow;
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
    wocky_xml:foldl_subels(FieldEl, [], fun read_tag/2).

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

add_owner(Owner, Fields) ->
    {ok, [#field{name = <<"owner">>, type = jid, value = jid:to_bare(Owner)} |
          Fields]}.

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
     field(<<"alerts">>,        int,    ?WOCKY_BOT_ALERT_DISABLED),
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


create(Owner, Server, Fields) ->
    do([error_m ||
        ID <- get_id(Owner, Fields),
        maybe_insert_name(ID, Fields),
        Fields2 <- add_defaults(Fields),
        Fields3 <- add_owner(Owner, Fields2),
        create_bot(Server, ID, Fields3),
        BotEl <- make_bot_el(Server, ID, Owner),
        {ok, {ID, BotEl}}
       ]).

get_id(Owner, Fields) ->
    case lists:keyfind(<<"id">>, #field.name, Fields) of
        false -> {ok, ?wocky_id:new()};
        #field{value = ID} -> check_id(Owner, ID)
    end.

check_id(Owner, ID) ->
    case wocky_db_bot:is_preallocated_id(Owner, ID) of
        true -> {ok, ID};
        false -> {error, ?ERR_ITEM_NOT_FOUND}
    end.

maybe_insert_name(ID, Fields) when is_list(Fields) ->
    case lists:keyfind(<<"shortname">>, #field.name, Fields) of
        false -> ok;
        #field{value = Val} -> insert_name(ID, Val)
    end.

insert_name(ID, Val) ->
    case wocky_db_bot:insert_new_name(ID, Val) of
        ok -> ok;
        {error, exists} -> {error, name_conflict_error()}
    end.

name_conflict_error() ->
    BaseStanza = jlib:stanza_error(<<"409">>, <<"modify">>,
                                   <<"undefined-condition">>),
    ConflictEl = #xmlel{name = <<"conflict">>,
                        attrs = [{<<"xmlns">>, ?NS_ERRORS}],
                        children = [#xmlel{name = <<"field">>,
                                           attrs = [{<<"var">>,
                                                     <<"shortname">>}]}]},
    BaseStanza#xmlel{children = [ConflictEl | BaseStanza#xmlel.children]}.

add_defaults(Fields) ->
    {ok, lists:foldl(fun maybe_add_default/2, Fields, optional_fields())}.

maybe_add_default(#field{name = Name, type = Type, value = Default}, Fields) ->
    case lists:keyfind(Name, #field.name, Fields) of
        false ->
            [#field{name = Name, type = Type, value = Default} | Fields];
        _ ->
            Fields
    end.

create_bot(Server, ID, Fields) ->
    FieldsMap = normalise_fields(Fields),
    insert_bot(Server, ID, FieldsMap#{updated => now}).

update_bot(Server, ID, Fields) ->
    FieldsMap = normalise_fields(Fields),
    insert_bot(Server, ID, FieldsMap).

insert_bot(Server, ID, FieldsMap) ->
    wocky_db_bot:insert(Server, FieldsMap#{id => ID}).

normalise_fields(Fields) ->
    lists:foldl(fun normalise_field/2, #{}, Fields).

normalise_field(#field{type = geoloc, value = {Lat, Lon}}, Acc) ->
    Acc#{lat => Lat, lon => Lon};
normalise_field(#field{name = N, type = jid, value = JID = #jid{}}, Acc) ->
    Acc#{binary_to_existing_atom(N, utf8) => jid:to_binary(JID)};
normalise_field(#field{name = N, value = V}, Acc) ->
    Acc#{binary_to_existing_atom(N, utf8) => V}.

make_bot_el(Server, ID, UserJID) ->
    case wocky_db_bot:get_bot(Server, ID) of
        not_found ->
            {error, ?ERR_ITEM_NOT_FOUND};
        Map ->
            make_bot_el(Map, UserJID)
    end.

make_bot_el(Bot, UserJID) ->
    RetFields = make_ret_elements(Bot, UserJID),
    {ok, make_ret_stanza(RetFields)}.

make_ret_elements(Map, UserJID) ->
    MetaFields = meta_fields(Map, UserJID),
    Fields = map_to_fields(Map),
    encode_fields(Fields ++ MetaFields).

meta_fields(#{id := ID, server := Server}, UserJID) ->
    Subscribers = wocky_db_bot:subscribers(Server, ID),
    ImageItems = wocky_db_bot:image_items_count(Server, ID),
    Subscribed = wocky_db_bot:is_subscribed(UserJID, ID),
    [make_field(<<"jid">>, jid, bot_jid(Server, ID)),
     make_field(<<"image_items">>, int, ImageItems),
     make_field(<<"subscribed">>, bool, Subscribed)
     | size_and_hash(<<"subscribers">>, Subscribers)].

bot_jid(Server, ID) ->
    jid:make(<<>>, Server, <<"bot/", ID/binary>>).

size_and_hash(Name, List) ->
    [make_field(<<Name/binary, "+size">>, int, length(List)),
     make_field(<<Name/binary, "+hash">>, string,
                wocky_bot_util:list_hash(List))
    ].

map_to_fields(Map = #{lat := Lat, lon := Lon}) ->
    [#field{name = <<"location">>, type = geoloc, value = {Lat, Lon}} |
     map_to_fields(maps:without([lat, lon], Map))];
map_to_fields(Map) ->
    maps:fold(fun to_field/3, [], Map).

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
    [field_element(N, string, V) | Acc];
encode_field(#field{name = N, type = jid, value = V}, Acc) ->
    [field_element(N, jid, safe_jid_to_binary(V)) | Acc];
encode_field(#field{name = N, type = int, value = V}, Acc) ->
    [field_element(N, int, integer_to_binary(V)) | Acc];
encode_field(#field{name = N, type = bool, value = V}, Acc) ->
    [field_element(N, bool, atom_to_binary(V, utf8)) | Acc];
encode_field(#field{name = N, type = geoloc, value = V}, Acc) ->
    [geoloc_field(N, V) | Acc];
encode_field(#field{name = N, type = timestamp, value = V}, Acc) ->
    [field_element(N, timestamp, wocky_db:timestamp_to_string(V)) | Acc];
encode_field(#field{name = N, type = tags, value = V}, Acc) ->
    [tags_element(N, V) | Acc].

make_jid(User) ->
    case jid:from_binary(User) of
        error -> {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Invalid user">>)};
        JID -> {ok, JID}
    end.

safe_jid_from_binary(<<>>) -> not_found;
safe_jid_from_binary(JID) -> jid:from_binary(JID).

safe_jid_to_binary(not_found) -> <<>>;
safe_jid_to_binary(JID) -> jid:to_binary(JID).

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
