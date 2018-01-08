%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module implementing Wocky publishing
%%% See https://github.com/hippware/tr-wiki/wiki/Publishing
%%%
-module(mod_wocky_publishing).

-compile({parse_transform, do}).
-compile({parse_transform, cut}).

-include("wocky.hrl").
-include("wocky_publishing.hrl").

-behaviour(gen_mod).

%% gen_mod handlers
-export([start/2, stop/1]).

%% IQ hook
-export([handle_iq/3]).

%% Notification callback
-export([send_notification/3]).


%%%===================================================================
%%% gen_mod handlers
%%%===================================================================

start(Host, _Opts) ->
    _ = ets:new(?PUBLISHING_HANDLER_TABLE,
                [named_table, public, {read_concurrency, true}]),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_PUBLISHING,
                                  ?MODULE, handle_iq, parallel),
    ejabberd_hooks:add(filter_local_packet, Host,
                       fun filter_local_packet_hook/1, 90),
    ejabberd_hooks:add(sm_remove_connection_hook, Host,
                       fun remove_connection_hook/4, 100),
    mod_disco:register_feature(Host, ?NS_PUBLISHING).

stop(Host) ->
    mod_disco:unregister_feature(Host, ?NS_PUBLISHING),
    ejabberd_hooks:delete(filter_local_packet, Host,
                          fun filter_local_packet_hook/1, 90),
    ejabberd_hooks:delete(sm_remove_connection_hook, Host,
                          fun remove_connection_hook/4, 100),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_PUBLISHING),
    ok.

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

handle_iq_type(From, To, #iq{type = set,
                             sub_el = #xmlel{name = <<"publish">>,
                                             attrs = Attrs,
                                             children = Children}
                            }) ->
    handle_publish(From, To, Attrs, Children);

handle_iq_type(From, To, #iq{type = get,
                             sub_el = #xmlel{name = <<"items">>,
                                             attrs = Attrs,
                                             children = Children}
                            }) ->
    handle_items(From, To, Attrs, Children);

handle_iq_type(From, To, #iq{type = get,
                             sub_el = #xmlel{name = <<"catchup">>,
                                             attrs = Attrs}
                            }) ->
    handle_catchup(From, To, Attrs);

handle_iq_type(_From, _To, _IQ) ->
    {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Invalid query">>)}.

%%%===================================================================
%%% Incoming packet handler
%%%===================================================================

-type filter_packet() :: {ejabberd:jid(), ejabberd:jid(), jlib:xmlel()}.
-spec filter_local_packet_hook(filter_packet() | drop) ->
    filter_packet() | drop.
filter_local_packet_hook(P = {From, To,
                              Packet = #xmlel{name = <<"presence">>}}) ->
    Type = presence_type(Packet),
    case handle_presence(From, To, Type, Packet) of
        drop -> drop;
        ignore -> P
    end;
filter_local_packet_hook(Other) ->
    Other.

%%%===================================================================
%%% Action handlers
%%%===================================================================

handle_publish(From, To, Attrs, Children) ->
    do([error_m ||
        check_same_user(From, To),
        TargetJID    <- get_target_jid(To, Attrs),
        Item         <- get_item_or_delete(Children),
        ID           <- get_id(Item#xmlel.attrs),
        Stanza       <- get_stanza(Item),
        wocky_publishing_handler:set(TargetJID, From, To, ID, Stanza),
        {NewItem, _, ExtraData}
            <- wocky_publishing_handler:get(TargetJID, From, ID),
        {ok, published_stanza(TargetJID, ID, NewItem, ExtraData)}
       ]).

handle_items(From, To, Attrs, Children) ->
    do([error_m ||
        check_same_user(From, To),
        TargetJID <- get_target_jid(To, Attrs),
        Param     <- get_item_id_or_rsm(Children),
        Result    <- wocky_publishing_handler:get(TargetJID, From, Param),
        result_stanza(Result, TargetJID)
       ]).

handle_catchup(From, To, Attrs) ->
    do([error_m ||
        check_same_user(From, To),
        TargetJID <- get_target_jid(To, Attrs),
        Version   <- wocky_xml:get_attr(<<"version">>, Attrs),
        Result    <- wocky_publishing_handler:catchup(TargetJID, From, Version),
        result_stanza(Result, TargetJID)
       ]).


handle_presence(_, _, unhandled_presence_type, _) ->
    ignore;

handle_presence(FromJID, ToJID, available, Packet) ->
    Result =
    do([error_m ||
        Query <- wocky_xml:get_subel(<<"query">>, Packet),
        wocky_xml:check_namespace(?NS_PUBLISHING, Query),
        {ok, Query}
       ]),

    case Result of
        {ok, Query} ->
            Version = get_version(Query#xmlel.attrs),
            handle_available(FromJID, ToJID, Query, Version);
        {error, _} ->
            ignore
    end;

% Explicit unsubscription
handle_presence(FromJID, ToJID, unavailable, _Packet) ->
    case wocky_publishing_handler:unsubscribe(ToJID, FromJID) of
        ok -> drop;
        {error, _} -> ignore
    end.

% Implicit unsubscription on disconnection unless the stream was resumed
remove_connection_hook(_SID, _JID, _Info, resumed) ->
    ok;
remove_connection_hook(_SID, JID, _Info, _Reason) ->
    wocky_publishing_handler:unsubscribe(all, JID).

%%%===================================================================
%%% Handler callbacks
%%%===================================================================

send_notification(ToJID, FromJID = #jid{lresource = LResource}, Item) ->
    Stanza = notification_stanza(LResource, item_stanza(Item)),
    ejabberd_router:route(FromJID, ToJID, Stanza),
    ok.

%%%===================================================================
%%% Private helpers
%%%===================================================================

handle_available(FromJID, ToJID, Query, Version) ->
    case wocky_publishing_handler:subscribe(ToJID, FromJID, Version) of
        ok ->
            drop;
        {error, ErrorStanza} ->
            Stanza = presence_error_stanza(Query, ErrorStanza),
            ejabberd_router:route(ToJID, FromJID, Stanza),
            drop;
        ignore ->
            ignore
    end.

check_same_user(A, B) ->
    case jid:are_bare_equal(A, B) of
        true -> ok;
        false -> {error, ?ERR_FORBIDDEN}
    end.

get_target_jid(To = #jid{lresource = LResource}, Attrs) ->
    case xml:get_attr(<<"node">>, Attrs) of
        {value, V} -> {ok, jid:replace_resource(To, V)};
        false ->
            case LResource of
                <<>> -> {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Missing node">>)};
                _ -> {ok, To}
            end
    end.

get_version(Attrs) ->
    case xml:get_attr(<<"version">>, Attrs) of
        false -> undefined;
        {value, V} -> V
    end.

get_item_or_delete(Children) ->
    case lists:keyfind(<<"item">>, #xmlel.name, Children) of
        false ->
            get_delete(Children);
        Item ->
            {ok, Item}
    end.

get_id(Attrs) ->
    case xml:get_attr(<<"id">>, Attrs) of
        false -> {ok, ?wocky_id:new()};
        {value, V} -> {ok, V}
    end.

get_delete(Children) ->
    case lists:keyfind(<<"delete">>, #xmlel.name, Children) of
        false ->
            {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Missing item or delete">>)};
        Delete ->
            {ok, Delete}
    end.

get_item_id_or_rsm(Children) ->
    Elem = #xmlel{name = <<>>, children = Children},
    case xml:get_path_s(Elem, [{elem, <<"item">>}, {attr, <<"id">>}]) of
        <<>> -> rsm_util:get_rsm(Elem);
        ID -> {ok, ID}
    end.

get_stanza(XML = #xmlel{name = <<"delete">>}) ->
    {ok, XML};
get_stanza(#xmlel{children = Children}) ->
    {ok, Children}.

published_stanza(#jid{lresource = LResource}, ID,
                 #published_item{version = Version},
                 ExtraData) ->
    #xmlel{name = <<"published">>,
           attrs = [{<<"xmlns">>, ?NS_PUBLISHING},
                    {<<"node">>, LResource}],
           children = [published_child(ID, Version) |
                       extra_data_child(ExtraData)]}.

published_child(ID, Version) ->
    #xmlel{name = <<"item">>,
           attrs = [{<<"id">>, ID},
                    {<<"version">>, Version}]}.

extra_data_child([]) -> [];
extra_data_child(ExtraData) ->
    [#xmlel{name = <<"extra-data">>, children = ExtraData}].

result_stanza(not_found, _TargetJID) ->
    {error, ?ERR_ITEM_NOT_FOUND};
result_stanza({#published_item{deleted = true}, _Version, _ExtraData},
              _TargetJID) ->
    {error, ?ERR_ITEM_NOT_FOUND};
result_stanza({Item = #published_item{}, Version, ExtraData}, TargetJID) ->
    items_stanza([item_stanza(Item)], Version, TargetJID, ExtraData);
result_stanza({Items, Version, ExtraData}, TargetJID) when is_list(Items) ->
    items_stanza(items_elements(Items), Version, TargetJID, ExtraData);
result_stanza({Items, Version, ExtraData, RSMOut}, TargetJID) ->
    items_stanza(items_elements(Items) ++ jlib:rsm_encode(RSMOut),
                 Version, TargetJID, ExtraData).

items_stanza(Children, Version, #jid{lresource = LResource}, ExtraData) ->
    {ok, #xmlel{name = <<"items">>,
                attrs = [{<<"xmlns">>, ?NS_PUBLISHING},
                         {<<"node">>, LResource} |
                         maybe_version_attr(Version)
                        ],
                children = Children ++ extra_data_child(ExtraData)}}.

items_elements(Items) ->
    lists:map(item_stanza(_), Items).

item_stanza(#published_item{id = ID,
                            version = Version,
                            deleted = true}) ->
    #xmlel{name = <<"delete">>,
           attrs = [{<<"id">>, ID},
                    {<<"version">>, Version}]};
item_stanza(#published_item{id = ID,
                            type = Type,
                            version = Version,
                            from = From,
                            stanza = Stanza}) ->
    #xmlel{name = Type,
           attrs = [{<<"id">>, ID},
                    {<<"version">>, Version},
                    {<<"from">>, jid:to_binary(From)}],
           children = maybe_wrap_list(Stanza)}.

maybe_wrap_list(X) when is_list(X) -> X;
maybe_wrap_list(X) -> [X].

presence_type(#xmlel{attrs = Attrs}) ->
    case xml:get_attr(<<"type">>, Attrs) of
        false -> available;
        {value, <<"available">>} -> available;
        {value, <<"unavailable">>} -> unavailable;
        _ -> unhandled_presence_type
    end.

notification_stanza(Node, ItemStanza) ->
    #xmlel{name = <<"message">>,
           attrs = [{<<"type">>, <<"headline">>}],
           children = [#xmlel{name = <<"notification">>,
                              attrs = [{<<"xmlns">>, ?NS_PUBLISHING},
                                       {<<"node">>, Node}],
                              children = [ItemStanza]}]}.

maybe_version_attr(not_found) -> [];
maybe_version_attr(Version) -> [{<<"version">>, Version}].

presence_error_stanza(Query, Error) ->
    #xmlel{name = <<"presence">>,
           attrs = [{<<"type">>, <<"error">>} | maybe_id(Query)],
           children = [Query, Error]}.

maybe_id(#xmlel{attrs = Attrs}) ->
    case xml:get_attr(<<"id">>, Attrs) of
        false -> [];
        {value, ID} -> [{<<"id">>, ID}]
    end.
