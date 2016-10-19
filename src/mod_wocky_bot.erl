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

-ignore_xref([handle_iq/3, check_access/3]).

%% gen_mod handlers
-export([start/2, stop/1]).

%% IQ handler
-export([handle_iq/3]).

%% Access manager callback
-export([check_access/3]).

-type loc() :: {float(), float()}.

-type field_type() :: string | int | geoloc | jid.

-record(field, {
          name :: binary(),
          type :: field_type(),
          value :: not_found | binary() | integer() | loc() | jid()
         }).

%%%===================================================================
%%% gen_mod handlers
%%%===================================================================

start(Host, _Opts) ->
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_BOT,
                                  ?MODULE, handle_iq, parallel),
    mod_disco:register_feature(Host, ?NS_BOT),
    ejabberd_hooks:add(filter_local_packet, Host,
                       fun filter_local_packet_hook/1, 80),
    mod_wocky_access:register(<<"bot">>, ?MODULE).

stop(Host) ->
    mod_disco:unregister_feature(Host, ?NS_BOT),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_BOT),
    ejabberd_hooks:delete(filter_local_packet, Host,
                          fun filter_local_packet_hook/1, 80),
    mod_wocky_access:unregister(<<"bot">>, ?MODULE).


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

% Retrieve
handle_iq_type(From, To, IQ = #iq{type = get,
                                  sub_el = #xmlel{name = <<"bot">>,
                                                  attrs = Attrs}
                                 }) ->
    handle_get(From, To, IQ, Attrs);

% Update
handle_iq_type(From, To, #iq{type = set,
                             sub_el = #xmlel{name = <<"fields">>,
                                             attrs = Attrs,
                                             children = Children}
                            }) ->
    handle_update(From, To, Attrs, Children);

% Retrieve affiliations
handle_iq_type(From, To, #iq{type = get,
                             sub_el = #xmlel{name = <<"affiliations">>,
                                             attrs = Attrs}
                            }) ->
    wocky_bot_users:handle_retrieve_affiliations(From, To, Attrs);

% Update affiliations
handle_iq_type(From, To, #iq{type = set,
                             sub_el = #xmlel{name = <<"affiliations">>,
                                             attrs = Attrs,
                                             children = Children}
                            }) ->
    wocky_bot_users:handle_update_affiliations(From, To, Attrs, Children);

% Subscribe
handle_iq_type(From, To, #iq{type = set,
                             sub_el = #xmlel{name = <<"subscribe">>,
                                             attrs = Attrs,
                                             children = Children}
                            }) ->
    wocky_bot_users:handle_subscribe(From, To, Attrs, Children);

% Unsubscribe
handle_iq_type(From, To, #iq{type = set,
                             sub_el = #xmlel{name = <<"unsubscribe">>,
                                             attrs = Attrs}
                            }) ->
    wocky_bot_users:handle_unsubscribe(From, To, Attrs);

% Retrieve subscribers
handle_iq_type(From, To, #iq{type = get,
                             sub_el = #xmlel{name = <<"subscribers">>,
                                             attrs = Attrs}
                            }) ->
    wocky_bot_users:handle_retrieve_subscribers(From, To, Attrs);

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

handle_iq_type(_From, _To, _IQ) ->
    {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Invalid query">>)}.

%%%===================================================================
%%% Action - create
%%%===================================================================

handle_create(From, Children) ->
    Server = wocky_app:server(),
    do([error_m ||
        Fields <- get_fields(Children),
        Fields2 <- add_server(Fields, Server),
        check_required_fields(Fields2, required_fields()),
        BotEl <- create_bot(From, Server, Fields2),
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
        delete_bot(Server, ID),
        {ok, []}
       ]).

delete_bot(Server, ID) ->
    #jid{luser= LUser, lserver = LServer} = wocky_db_bot:owner(Server, ID),
    wocky_db_user:remove_roster_viewer(LUser, LServer, bot_jid(Server, ID)),
    wocky_db_bot:delete(Server, ID),
    ok.

%%%===================================================================
%%% Action - get
%%%===================================================================

handle_get(From, #jid{lserver = Server}, IQ, Attrs) ->
    case wocky_bot_util:get_id_from_node(Attrs) of
        {ok, ID} -> get_bot_by_id(From, Server, ID);
        {error, _} -> get_bots_for_user(From, Server, IQ, Attrs)
    end.

get_bot_by_id(From, Server, ID) ->
    do([error_m ||
        wocky_bot_util:check_access(Server, ID, From),
        BotEl <- make_bot_el(Server, ID),
        {ok, BotEl}
       ]).

get_bots_for_user(From, Server, IQ, Attrs) ->
    do([error_m ||
        User <- wocky_xml:get_attr(<<"user">>, Attrs),
        RSMIn <- rsm_util:get_rsm(IQ),
        {Bots, RSMOut} <- users_bots(Server, From, User, RSMIn),
        {ok, users_bots_result(Bots, RSMOut)}
       ]).

users_bots(Server, From, User, RSMIn) ->
    BotIDs = wocky_db_bot:get_by_user(Server, User),
    VisibleIDs = lists:filter(access_filter(Server, From, _), BotIDs),
    % We don't have any particular order we want the bots in, it just
    % needs to be consistant
    SortedIDs = lists:sort(VisibleIDs),
    {FilteredIDs, RSMOut} = rsm_util:filter_with_rsm(SortedIDs, RSMIn),
    Bots = [wocky_db_bot:get(Server, ID) || ID <- FilteredIDs],
    {ok, {Bots, RSMOut}}.

access_filter(Server, From, ID) ->
    ok =:= wocky_bot_util:check_access(Server, ID, From).

users_bots_result(Bots, RSMOut) ->
    BotEls = make_bot_els(Bots),
    #xmlel{name = <<"bots">>,
           attrs = [{<<"xmlns">>, ?NS_BOT}],
           children = BotEls ++ jlib:rsm_encode(RSMOut)}.

make_bot_els(BotIDs) ->
    lists:reverse(
      lists:foldl(make_bot_els(_, _), [], BotIDs)).

make_bot_els(ID, Acc) ->
    {ok, El} = make_bot_el(ID),
    [El|Acc].

%%%===================================================================
%%% Action - update
%%%===================================================================

handle_update(From, #jid{lserver = Server}, Attrs, Children) ->
    do([error_m ||
        ID <- wocky_bot_util:get_id_from_node(Attrs),
        wocky_bot_util:check_owner(Server, ID, From),
        Fields <- get_fields(Children),
        update_bot(Server, ID, Fields),
        {ok, []}
       ]).

%%%===================================================================
%%% Incoming packet handler
%%%===================================================================

-type filter_packet() :: {ejabberd:jid(), ejabberd:jid(), jlib:xmlel()}.
-spec filter_local_packet_hook(filter_packet() | drop) ->
    filter_packet() | drop.
filter_local_packet_hook(P = {From,
                              #jid{user = <<>>, lserver = LServer,
                                   resource= <<"bot/", BotID/binary>>},
                              Packet}) ->
    case handle_bot_packet(From, LServer, BotID, Packet) of
        ok -> drop;
        ignored -> P
    end;
filter_local_packet_hook(Other) ->
    Other.

handle_bot_packet(From, LServer, BotID,
                  #xmlel{name = <<"message">>,
                         attrs = Attrs, children = Children}) ->
    case xml:get_attr(<<"type">>, Attrs) of
        {value, <<"headline">>} ->
            handle_roster_changed(From, LServer, BotID, Children);
        false ->
            ignored
    end;

handle_bot_packet(_, _, _, _) ->
    ignored.

%%%===================================================================
%%% Roster update packet handler
%%%===================================================================

-record(roster_item, {
          jid :: ejabberd:jid(),
          subscription :: subscription_type(),
          groups :: [binary()]
         }).

handle_roster_changed(From, LServer, BotID,
                     [El = #xmlel{name = <<"roster-changed">>,
                                  children = Children}]) ->
    _ = do([error_m ||
            wocky_xml:check_namespace(?NS_WOCKY_ROSTER, El),
            wocky_bot_util:check_owner(LServer, BotID, From),
            RemovedJIDs <- get_removed_jids(Children),
            Bot <- {ok, wocky_db_bot:get(LServer, BotID)},
            remove_invalidated_associations(LServer, BotID, Bot, RemovedJIDs)
           ]),
    ok;
handle_roster_changed(_, _, _, _) ->
    ignored.

get_removed_jids(ItemEls) ->
    Items = els_to_items(ItemEls),
    UnFriended = lists:filter(fun(I) -> not is_friend(I) end, Items),
    {ok, [JID || #roster_item{jid = JID} <- UnFriended]}.

is_friend(#roster_item{subscription = Sub, groups = Groups}) ->
    wocky_util:is_friend(Sub, Groups).

els_to_items(ItemEls) ->
    lists:foldl(fun el_to_item/2, [], ItemEls).

el_to_item(#xmlel{name = <<"item">>, attrs = Attrs, children = Children},
           Acc) ->
    Subscription = binary_to_existing_atom(
                     xml:get_attr_s(<<"subscription">>, Attrs), utf8),
    Groups = get_groups(Children),
    {value, JID} = xml:get_attr(<<"jid">>, Attrs),
    [#roster_item{jid = jid:from_binary(JID),
                  groups = Groups,
                  subscription = Subscription}
     | Acc];
el_to_item(_, Acc) -> Acc.

get_groups(Elements) ->
    lists:foldl(fun get_group/2, [], Elements).

get_group(#xmlel{name = <<"group">>,
                 children = [#xmlcdata{content = Group}]}, Acc) ->
    [Group | Acc];
get_group(_, Acc) -> Acc.

remove_invalidated_associations(LServer, ID,
                                #{visibility := Visibility},
                                RemovedJIDs) ->
    remove_invalidated_affiliates(LServer, ID, RemovedJIDs),
    remove_invalidated_subscribers(LServer, ID, Visibility, RemovedJIDs).

remove_invalidated_affiliates(LServer, ID, RemovedJIDs) ->
    OldAffiliates = [A || {A, _} <- wocky_db_bot:affiliations(LServer, ID)],
    RemovedAffiliates = wocky_util:intersection(RemovedJIDs, OldAffiliates),
    RemovedAffiliations = [{I, none} || I <- RemovedAffiliates],
    wocky_db_bot:update_affiliations(LServer, ID, RemovedAffiliations),
    wocky_bot_util:notify_affiliates(
      jid:make(<<>>, LServer, <<>>), ID, RemovedAffiliations).

remove_invalidated_subscribers(LServer, ID, Vis, RemovedJIDs)
  when Vis =:= ?WOCKY_BOT_VIS_FRIENDS;
       Vis =:= ?WOCKY_BOT_VIS_WHITELIST ->
    lists:foreach(remove_invalidated_subscriber(LServer, ID, _),
                  RemovedJIDs);
remove_invalidated_subscribers(_, _, _, _) ->
    ok.

remove_invalidated_subscriber(LServer, ID, JID) ->
    case wocky_db_bot:follow_state(LServer, ID, JID) of
        not_found ->
            ok;
        Follow ->
            wocky_db_bot:unsubscribe(LServer, ID, JID),
            notify_unsubscribe(LServer, ID, JID, Follow)
    end.

notify_unsubscribe(LServer, ID, JID, Follow) ->
    Stanza =
    #xmlel{name = <<"message">>,
           children = [make_unsubscribed(ID, Follow)]},
    ejabberd_router:route(jid:make(<<>>, LServer, <<>>), JID, Stanza).

make_unsubscribed(ID, Follow) ->
    #xmlel{name = <<"unsubscribed">>,
           attrs = [{<<"xmlns">>, ?NS_BOT},
                    {<<"node">>, wocky_bot_util:make_node(ID)}],
           children = [wocky_bot_util:make_follow_element(Follow)]}.

%%%===================================================================
%%% Access manager callback
%%%===================================================================

-spec check_access(binary(), ejabberd:jid(), mod_wocky_access:op()) ->
    mod_wocky_access:access_result().
check_access(<<"bot/", ID/binary>>, Actor, view) ->
    allow_to_result(
      wocky_db_bot:has_access(wocky_app:server(), ID, Actor));

check_access(<<"bot/", ID/binary>>, Actor, _) ->
    case wocky_db_bot:owner(wocky_app:server(), ID) of
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
    wocky_xml:act_on_subel(<<"geoloc">>, FieldEl, fun read_geoloc/1).

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
    [field(<<"description">>,   string, <<>>),
     field(<<"shortname">>,     string, <<>>),
     field(<<"image">>,         string, <<>>),
     field(<<"type">>,          string, <<>>),
     field(<<"address">>,       string, <<>>),
     field(<<"visibility">>,    int,    ?WOCKY_BOT_VIS_OWNER),
     field(<<"alerts">>,        int,    ?WOCKY_BOT_ALERT_DISABLED)].

output_only_fields() ->
    [field(<<"id">>,            string, <<>>),
     field(<<"server">>,        string, <<>>),
     field(<<"owner">>,         jid,    <<>>)].

create_fields() -> required_fields() ++ optional_fields().
output_fields() -> required_fields() ++ optional_fields()
                   ++ output_only_fields().

field(Name, Type, Value) ->
    #field{name = Name, type = Type, value = Value}.


create_bot(Owner, Server, Fields) ->
    ID = wocky_db:create_id(),
    do([error_m ||
        maybe_insert_name(ID, Fields),
        Fields2 <- add_defaults(Fields),
        Fields3 <- add_owner(Owner, Fields2),
        update_bot(Server, ID, Fields3),
        add_bot_as_roster_viewer(Owner, Server, ID),
        make_bot_el(Server, ID)
       ]).

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

update_bot(Server, ID, Fields) ->
    NormalisedFields = lists:foldl(fun normalise_field/2, #{}, Fields),
    wocky_db_bot:insert(Server, NormalisedFields#{id => ID}).

normalise_field(#field{type = geoloc, value = {Lat, Lon}}, Acc) ->
    Acc#{lat => Lat, lon => Lon};
normalise_field(#field{name = N, type = jid, value = JID = #jid{}}, Acc) ->
    Acc#{binary_to_existing_atom(N, utf8) => jid:to_binary(JID)};
normalise_field(#field{name = N, value = V}, Acc) ->
    Acc#{binary_to_existing_atom(N, utf8) => V}.

add_bot_as_roster_viewer(#jid{luser = LUser, lserver = LServer}, Server, ID) ->
    wocky_db_user:add_roster_viewer(LUser, LServer, bot_jid(Server, ID)),
    ok.

make_bot_el(Server, ID) ->
    case wocky_db_bot:get(Server, ID) of
        not_found ->
            {error, ?ERR_ITEM_NOT_FOUND};
        Map ->
            make_bot_el(Map)
    end.

make_bot_el(Bot) ->
    RetFields = make_ret_elements(Bot),
    {ok, make_ret_stanza(RetFields)}.

make_ret_elements(Map) ->
    MetaFields = meta_fields(Map),
    Fields = map_to_fields(Map),
    encode_fields(Fields ++ MetaFields).

meta_fields(Map = #{id := ID, server := Server}) ->
    Subscribers = wocky_db_bot:subscribers(Server, ID),
    Followers = lists:filter(fun({_JID, Follow}) -> Follow end, Subscribers),
    Affiliates = wocky_db_bot:affiliations_from_map(Map),
    ImageItems = wocky_db_bot:image_items_count(Server, ID),
    [make_field(<<"jid">>, jid, bot_jid(Server, ID)),
     make_field(<<"image_items">>, int, ImageItems) |
     size_and_hash(<<"followers">>, Followers) ++
     size_and_hash(<<"affiliates">>, Affiliates) ++
     size_and_hash(<<"subscribers">>, Subscribers)].

bot_jid(Server, ID) ->
    jid:make(<<>>, Server, <<"bot/", ID/binary>>).

size_and_hash(Name, not_found) -> size_and_hash(Name, []);
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

make_field(Name, Type, Val) when Type =:= string orelse Type =:= int ->
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
encode_field(#field{name = N, type = geoloc, value = V}, Acc) ->
    [geoloc_field(N, V) | Acc].

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
    #xmlel{name = <<"value">>,
           children = [#xmlcdata{content = Val}]}.

geoloc_field(Name, Val) ->
    #xmlel{name = <<"field">>,
           attrs = [{<<"var">>, Name},
                    {<<"type">>, <<"geoloc">>}],
           children = [geoloc_element(Val)]}.

geoloc_element({Lat, Lon}) ->
    #xmlel{name = <<"geoloc">>,
           attrs = [{<<"xmlns">>, ?NS_GEOLOC}],
           children = [float_element(N, V) || {N, V} <- [{<<"lat">>, Lat},
                                                         {<<"lon">>, Lon}]]}.

float_element(Name, Val) ->
    #xmlel{name = Name,
           children = [#xmlcdata{content=wocky_util:coord_to_binary(Val)}]}.

make_ret_stanza(Fields) ->
    #xmlel{name = <<"bot">>,
           attrs = [{<<"xmlns">>, ?NS_BOT}],
           children = Fields}.
