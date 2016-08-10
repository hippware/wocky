%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module implementing Wocky bots
%%% See https://github.com/hippware/tr-wiki/wiki/Bot
%%%
-module(mod_bot).

-behaviour(gen_mod).

-compile({parse_transform, do}).
-compile({parse_transform, cut}).
-compile({parse_transform, fun_chain}).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").
-include("wocky.hrl").

-ignore_xref([{handle_iq, 3}]).

%% gen_mod handlers
-export([start/2, stop/1]).

%% IQ hook
-export([handle_iq/3]).

%% Internal use
-export([normalise_field/2,
         encode_field/2,
         to_field/3,
         make_affiliate_element/1,
         get_affiliation/2,
         make_follower_element/1
        ]).

-type loc() :: {float(), float()}.

-type field_type() :: string | int | geoloc.

-record(field, {
          name :: binary(),
          type :: field_type(),
          value :: binary() | integer() | loc()
         }).


%%%===================================================================
%%% gen_mod handlers
%%%===================================================================

start(Host, _Opts) ->
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_BOT,
                                  ?MODULE, handle_iq, parallel),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_BOT,
                                  ?MODULE, handle_iq, parallel),
    mod_disco:register_feature(Host, ?NS_BOT).

stop(Host) ->
    mod_disco:unregister_feature(Host, ?NS_BOT),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_BOT),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_BOT).


%%%===================================================================
%%% Event handler
%%%===================================================================

-spec handle_iq(From :: ejabberd:jid(),
                To :: ejabberd:jid(),
                IQ :: iq()) -> iq().
handle_iq(From, To, IQ) ->
    case handle_iq_type(From, To, IQ) of
        {ok, ResponseIQ} -> ResponseIQ;
        {error, Error} -> wocky_util:make_error_iq_response(IQ, Error)
    end.

% Create
handle_iq_type(From, _To, IQ = #iq{type = set,
                                   sub_el = #xmlel{name = <<"create">>,
                                                   children = Children}
                             }) ->
    handle_create(From, IQ, Children);

% Delete
handle_iq_type(From, _To, IQ = #iq{type = set,
                                   sub_el = #xmlel{name = <<"delete">>,
                                                   attrs = Attrs}
                                  }) ->
    handle_delete(From, IQ, Attrs);

% Retrieve
handle_iq_type(From, To, IQ = #iq{type = get,
                                   sub_el = #xmlel{name = <<"bot">>,
                                                   attrs = Attrs}
                                  }) ->
    handle_get(From, To, IQ, Attrs);

% Update
handle_iq_type(From, To, IQ = #iq{type = set,
                                   sub_el = #xmlel{name = <<"bot">>,
                                                   attrs = Attrs,
                                                   children = Children}
                                  }) ->
    handle_update(From, To, IQ, Attrs, Children);

% Retrieve affiliations
handle_iq_type(From, To, IQ = #iq{type = get,
                                   sub_el = #xmlel{name = <<"affiliations">>,
                                                   attrs = Attrs}
                                  }) ->
    handle_retrieve_affiliations(From, To, IQ, Attrs);

% Update affiliations
handle_iq_type(From, To, IQ = #iq{type = set,
                                   sub_el = #xmlel{name = <<"affiliations">>,
                                                   attrs = Attrs,
                                                   children = Children}
                                  }) ->
    handle_update_affiliations(From, To, IQ, Attrs, Children);

% Follow
handle_iq_type(From, To, IQ = #iq{type = set,
                                   sub_el = #xmlel{name = <<"follow">>,
                                                   attrs = Attrs}
                                  }) ->
    handle_follow(From, To, IQ, Attrs);

% Unfollow
handle_iq_type(From, To, IQ = #iq{type = set,
                                   sub_el = #xmlel{name = <<"unfollow">>,
                                                   attrs = Attrs}
                                  }) ->
    handle_unfollow(From, To, IQ, Attrs);

% Retrive followers
handle_iq_type(From, To, IQ = #iq{type = get,
                                   sub_el = #xmlel{name = <<"followers">>,
                                                   attrs = Attrs}
                                  }) ->
    handle_retrieve_followers(From, To, IQ, Attrs);

handle_iq_type(_From, _To, _IQ) ->
    {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Invalid query">>)}.

%%%===================================================================
%%% Action - create
%%%===================================================================

handle_create(From, IQ, Children) ->
    do([error_m ||
        Fields <- get_fields(Children),
        AllFields <- add_owner(From, Fields),
        check_required_fields(AllFields, required_fields()),
        BotEl <- create_bot(From, AllFields),
        {ok, IQ#iq{type = result, sub_el = BotEl}}
       ]).

%%%===================================================================
%%% Action - delete
%%%===================================================================

handle_delete(From, IQ, Attrs) ->
    do([error_m ||
        BotJID <- get_attr(<<"jid">>, Attrs),
        {Server, ID} <- get_id_server_from_jid(BotJID),
        check_owner(Server, ID, From),
        delete_bot(Server, ID),
        {ok, IQ#iq{type = result, sub_el = []}}
       ]).

delete_bot(Server, ID) ->
    {ok, wocky_db_bot:delete(Server, ID)}.

%%%===================================================================
%%% Action - get
%%%===================================================================

handle_get(From, #jid{lserver = Server}, IQ, Attrs) ->
    do([error_m ||
        BotNode <- get_attr(<<"node">>, Attrs),
        ID <- get_id_from_node(BotNode),
        check_access(Server, ID, From),
        BotEl <- make_bot_el(Server, ID),
        {ok, IQ#iq{type = result, sub_el = BotEl}}
       ]).

check_access(Server, ID, From) ->
    case wocky_db_bot:has_access(Server, ID, From) of
        true -> ok;
        false -> {error, ?ERR_FORBIDDEN};
        not_found -> {error, ?ERR_ITEM_NOT_FOUND}
    end.

%%%===================================================================
%%% Action - update
%%%===================================================================

handle_update(From, #jid{lserver = Server}, IQ, Attrs, Children) ->
    do([error_m ||
        BotNode <- get_attr(<<"node">>, Attrs),
        ID <- get_id_from_node(BotNode),
        check_owner(Server, ID, From),
        Fields <- get_fields(Children),
        update_bot(Server, ID, Fields),
        {ok, IQ#iq{type = result, sub_el = []}}
       ]).

%%%===================================================================
%%% Action - retrieve affiliations
%%%===================================================================

handle_retrieve_affiliations(From, #jid{lserver = Server}, IQ, Attrs) ->
    do([error_m ||
        BotNode <- get_attr(<<"node">>, Attrs),
        ID <- get_id_from_node(BotNode),
        check_owner(Server, ID, From),
        AffiliationsEl <- make_affiliations_element(Server, ID),
        {ok, IQ#iq{type = result, sub_el = AffiliationsEl}}
       ]).

make_affiliations_element(Server, ID) ->
    Affiliates = wocky_db_bot:affiliations(Server, ID),
    {ok, #xmlel{name = <<"affiliations">>,
                attrs = list_attrs(ID, Affiliates),
                children = make_affiliate_elements(Affiliates)}}.

make_affiliate_elements(Affiliates) ->
    lists:map(fun make_affiliate_element/1, Affiliates).

%%%===================================================================
%%% Action - update affiliations
%%%===================================================================

handle_update_affiliations(From, To = #jid{lserver = Server},
                           IQ, Attrs, Children) ->
    do([error_m ||
        BotNode <- get_attr(<<"node">>, Attrs),
        ID <- get_id_from_node(BotNode),
        check_owner(Server, ID, From),
        DirtyAffiliations <- get_affiliations(Children),
        Affiliations <- check_affiliations(From, DirtyAffiliations, []),
        update_affiliations(Server, ID, Affiliations),
        notify_affiliates(To, ID, Affiliations),
        {ok, IQ#iq{type = result,
                   sub_el = make_affiliations_update_element(Server, ID)}}
       ]).

get_affiliations(Elements) ->
    lists:foldl(fun get_affiliation/2, [], Elements).

get_affiliation(El = #xmlel{name = <<"affiliation">>}, Acc) ->
    [element_to_affiliation(El) | Acc];
get_affiliation(_, Acc) -> Acc.

element_to_affiliation(#xmlel{attrs = Attrs}) ->
    JID = xml:get_attr_s(<<"jid">>, Attrs),
    Affiliation = xml:get_attr_s(<<"affiliation">>, Attrs),
    {JID, Affiliation}.

check_affiliations(_From, [], Acc) -> {ok, Acc};
check_affiliations(From, [Affiliation | Rest], Acc) ->
    case check_affiliation(From, Affiliation) of
        {error, E} ->
            {error, E};
        CleanAffiliation ->
            check_affiliations(From, Rest, [CleanAffiliation | Acc])
    end.

check_affiliation(_From, {User, <<"none">>}) ->
    {User, none};
check_affiliation(From, {User, <<"spectator">>}) ->
    case wocky_db_roster:has_contact(From, jid:from_binary(User)) of
        true ->
            {User, spectator};
        false ->
            {error, ?ERRT_BAD_REQUEST(
                       ?MYLANG, <<User/binary, " is not a contact">>)}
    end;
check_affiliation(_, {_User, Role}) ->
    {error, ?ERRT_BAD_REQUEST(
               ?MYLANG, <<"Invalid affiliate role: ", Role/binary>>)}.

update_affiliations(Server, ID, Affiliations) ->
    wocky_db_bot:update_affiliations(Server, ID, Affiliations).

notify_affiliates(Sender, ID, Affiliates) ->
    lists:foreach(notify_affiliate(Sender, ID, _), Affiliates).

notify_affiliate(Sender, ID, {User, Role}) ->
    ejabberd_router:route(Sender, jid:from_binary(User),
                          make_update_packet(ID, User, Role)).

make_update_packet(ID, User, Role) ->
    #xmlel{name = <<"message">>,
           children = [#xmlel{name = <<"affiliations">>,
                              attrs = [{<<"xmlns">>, ?NS_BOT},
                                       {<<"node">>, make_node(ID)}],
                              children =
                                [make_affiliate_element({User, Role})]}]}.

make_affiliations_update_element(Server, ID) ->
    Affiliations = wocky_db_bot:affiliations(Server, ID),
    #xmlel{name = <<"affiliations">>,
           attrs = list_attrs(ID, Affiliations)}.

%%%===================================================================
%%% Action - follow
%%%===================================================================

handle_follow(From, #jid{lserver = Server}, IQ, Attrs) ->
    do([error_m ||
        BotNode <- get_attr(<<"node">>, Attrs),
        ID <- get_id_from_node(BotNode),
        check_access(Server, ID, From),
        follow_bot(Server, ID, From),
        {ok, IQ#iq{type = result, sub_el = []}}
       ]).

follow_bot(Server, ID, From) ->
    {ok, wocky_db_bot:follow(Server, ID, From)}.

%%%===================================================================
%%% Action - unfollow
%%%===================================================================

handle_unfollow(From, #jid{lserver = Server}, IQ, Attrs) ->
    do([error_m ||
        BotNode <- get_attr(<<"node">>, Attrs),
        ID <- get_id_from_node(BotNode),
        check_bot_exists(Server, ID),
        unfollow_bot(Server, ID, From),
        {ok, IQ#iq{type = result, sub_el = []}}
       ]).

unfollow_bot(Server, ID, From) ->
    {ok, wocky_db_bot:unfollow(Server, ID, From)}.

%%%===================================================================
%%% Action - retrieve followers
%%%===================================================================

handle_retrieve_followers(From, #jid{lserver = Server}, IQ, Attrs) ->
    do([error_m ||
        BotNode <- get_attr(<<"node">>, Attrs),
        ID <- get_id_from_node(BotNode),
        check_owner(Server, ID, From),
        AffiliationsEl <- make_followers_element(Server, ID),
        {ok, IQ#iq{type = result, sub_el = AffiliationsEl}}
       ]).

make_followers_element(Server, ID) ->
    Followers = wocky_db_bot:followers(Server, ID),
    {ok, #xmlel{name = <<"followers">>,
                attrs = list_attrs(ID, Followers),
                children = make_followers_element(Followers)}}.

make_followers_element(Followers) ->
    lists:map(fun make_follower_element/1, Followers).

make_follower_element(JID) ->
    #xmlel{name = <<"affiliation">>,
           attrs = [{<<"jid">>, JID}]}.


%%%===================================================================
%%% Common helpers
%%%===================================================================

get_id_server_from_jid(JIDBin) ->
    JID = jid:from_binary(JIDBin),
    case get_id_from_node(JID#jid.lresource) of
        {ok, ID} -> {ok, {JID#jid.lserver, ID}};
        Error -> Error
    end.

get_id_from_node(Node) ->
    case binary:split(Node, <<$/>>, [global]) of
        [<<"bot">>, ID] -> {ok, ID};
        _ -> {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Invalid bot node">>)}
    end.

check_bot_exists(Server, ID) ->
    case wocky_db_bot:exists(Server, ID) of
        true -> ok;
        false -> {error, ?ERR_ITEM_NOT_FOUND}
    end.

check_owner(Server, ID, User) ->
    UserBin = jid:to_binary(User),
    case wocky_db_bot:owner(Server, ID) of
        UserBin -> ok;
        _ -> {error, ?ERR_ITEM_NOT_FOUND}
    end.

get_fields(Children) ->
    get_fields(Children, []).

get_fields(_, [{error, E} | _]) -> {error, E};
get_fields([], Acc) -> {ok, Acc};
get_fields([El = #xmlel{name = <<"field">>,
                        attrs = Attrs}
            | Rest] , Acc) ->
    F = do([error_m ||
            Name <- get_attr(<<"val">>, Attrs),
            TypeBin <- get_attr(<<"type">>, Attrs),
            Type <- check_field(Name, TypeBin),
            Value <- get_field_value(Type, El),
            #field{name = Name, type = Type, value = Value}
           ]),
    get_fields(Rest, [F | Acc]).

get_attr(AttrName, Attrs) ->
    case xml:get_attr(AttrName, Attrs) of
        {value, Val} ->
            {ok, Val};
        false ->
            {error, ?ERRT_BAD_REQUEST(?MYLANG,
                                      <<"Missing ", AttrName/binary,
                                        " attribute">>)}
    end.

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
        check_namespace(?NS_GEOLOC, GeolocEl),
        Lat <- wocky_xml:act_on_subel_cdata(
                 <<"lat">>, GeolocEl, fun read_float/1),
        Lon <- wocky_xml:act_on_subel_cdata(
                 <<"lon">>, GeolocEl, fun read_float/1),
        {ok, {Lat, Lon}}
       ]).

add_owner(Owner, Fields) ->
    OwnerJidBin = jid:to_binary(jid:to_bare(Owner)),
    {ok, [#field{name = <<"owner">>, type = string, value = OwnerJidBin} |
          Fields]}.

check_namespace(NS, #xmlel{attrs = Attrs}) ->
    case xml:get_attr_s(<<"xmlns">>, Attrs) of
        NS ->
            ok;
        X ->
            {error, ?ERRT_BAD_REQUEST(?MYLANG,
                                      <<"Invlid namespace: ", X/binary>>)}
    end.

check_required_fields(_Fields, []) -> ok;
check_required_fields(Fields, [{Name, _} | Rest]) ->
    case lists:any(fun({FName, _, _}) -> FName =:= Name end, Fields) of
        true ->
            check_required_fields(Fields, Rest);
        false ->
            {error, ?ERRT_BAD_REQUEST(?MYLANG,
                                      <<"Missing ", Name/binary, "field">>)}
    end.

check_field(Name, TypeBin) ->
    ExpectedType = proplists:get_value(Name, fields()),
    ExpectedTypeBin = atom_to_binary(ExpectedType, utf8),
    case ExpectedTypeBin of
        <<"undefined">> ->
            {error, ?ERRT_BAD_REQUEST(
                       ?MYLANG, <<"Invalid field ", Name/binary>>)};
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
    %% Name,                Type
    [{<<"title">>,          string},
     {<<"shortname">>,      string},
     {<<"location">>,       geoloc},
     {<<"radius">>,         int}].

optional_fields() ->
    [{<<"description">>,    string},
     {<<"visibility">>,     int},
     {<<"alerts">>,         int}].

fields() -> required_fields() ++ optional_fields().

create_bot(Owner, Fields) ->
    ID = wocky_db:create_id(),
    do([error_m ||
        maybe_insert_name(ID, Fields),
        insert_bot(wocky_app:server(), ID, Owner, Fields),
        {ok, make_bot_el(wocky_app:server(), ID)}
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

insert_bot(Server, ID, Owner, Fields) ->
    FullFields = [{<<"owner">>, Owner} | Fields],
    update_bot(Server, ID, FullFields).

update_bot(Server, ID, Fields) ->
    NormalisedFields = lists:foldl(fun normalise_field/2, #{}, Fields),
    wocky_db_bot:insert(Server, NormalisedFields#{id => ID}).

normalise_field(#field{type = geoloc, value = {Lat, Lon}}, Acc) ->
    Acc#{lat => Lat, lon => Lon};
normalise_field(#field{name = N, value = V}, Acc) ->
    Acc#{binary_to_existing_atom(N, utf8) => V}.

make_bot_el(Server, ID) ->
    case wocky_db_bot:get(Server, ID) of
        not_found ->
            {error, ?ERR_ITEM_NOT_FOUND};
        Map ->
            RetFields = make_ret_elements(Map),
            {ok, make_ret_stanza(RetFields)}
    end.

make_ret_elements(Map) ->
    MetaFields = meta_fields(Map),
    Fields = encode_fields(Map),
    Fields ++ MetaFields.

meta_fields(Map = #{id := ID, server := Server, followers := Followers}) ->
    Affiliates = wocky_db_bot:affiliations_from_map(Map),
    [#field{name = <<"jid">>, type = string, value = bot_jid(ID, Server)} |
     size_and_hash(<<"affiliates">>, Affiliates) ++
     size_and_hash(<<"followers">>, Followers)].

bot_jid(ID, Server) ->
    <<Server/binary, "/bot/", ID/binary>>.

size_and_hash(Name, List) ->
    [#field{name = <<Name/binary, "+size">>, type = int,
            value = length(List)},
     #field{name = <<Name/binary, "+hash">>, type = string,
            value = list_hash(List)}
    ].

list_hash(List) ->
    %% TODO: replace with murmurhash once new cqerl is used
    fun_chain:last(
      List,
      term_to_binary(),
      crypto:hash(md5),
      base64:encode()
     ).

encode_fields(Map) ->
    Fields = maps:fold(fun to_field/3, [], Map),
    lists:foldl(fun encode_field/2, [], Fields).

to_field(Key, Val, Acc) ->
    KeyBin = atom_to_binary(Key, utf8),
    case proplists:get_value(KeyBin, fields()) of
        undefined -> Acc;
        Type -> [#field{name = KeyBin, type = Type, value = Val} | Acc]
    end.

encode_field(#field{name = N, type = Type, value = V}, Acc)
    when Type =:= string; Type =:= jid ->
    [field_element(N, Type, V) | Acc];
encode_field(#field{name = N, type = int, value = V}, Acc) ->
    [field_element(N, int, integer_to_binary(V)) | Acc];
encode_field(#field{name = N, type = geoloc, value = V}, Acc) ->
    [geoloc_field(N, V) | Acc].

field_element(Name, Type, Val) ->
    #xmlel{name = <<"field">>,
           attrs = [{<<"var">>, Name},
                    {<<"type">>, atom_to_binary(Type, utf8)}],
           children = [#xmlcdata{content = Val}]}.

geoloc_field(Name, Val) ->
    #xmlel{name = <<"field">>,
           attrs = [{<<"var">>, Name},
                    {<<"type">>, <<"geoloc">>}],
           children = [geoloc_element(Val)]}.

geoloc_element({Lat, Lon}) ->
    #xmlel{name = <<"geoloc">>,
           attrs = [{<<"xmlns">>, ?NS_GEOLOC}],
           children = [float_el(N, V) || {N, V} <- [{<<"lat">>, Lat},
                                                    {<<"lon">>, Lon}]]}.

float_el(Name, Val) ->
    #xmlel{name = Name,
           children = [#xmlcdata{content=float_to_binary(Val)}]}.

make_ret_stanza(Fields) ->
    #xmlel{name = <<"bot">>,
           attrs = [{<<"xmlns">>, ?NS_BOT}],
           children = Fields}.

make_affiliate_element({JID, Affiliation}) ->
    #xmlel{name = <<"affiliation">>,
           attrs = [{<<"jid">>, JID},
                    {<<"affiliation">>, atom_to_binary(Affiliation, utf8)}]}.

list_attrs(ID, List) ->
    [{<<"xmlns">>, ?NS_BOT},
     {<<"node">>, make_node(ID)},
     {<<"size">>, integer_to_binary(length(List))},
     {<<"hash">>, list_hash(List)}].

make_node(ID) ->
    <<"bot/", ID/binary>>.

