%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module implementing Wocky publishing
%%% See https://github.com/hippware/tr-wiki/wiki/Publishing
%%%
-module(mod_wocky_publishing).

-behaviour(gen_mod).

-compile({parse_transform, do}).
-compile({parse_transform, cut}).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").
-include("wocky.hrl").
-include("wocky_publishing.hrl").

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
                       fun filter_local_packet_hook/1, 80),
    mod_disco:register_feature(Host, ?NS_PUBLISHING).

stop(Host) ->
    mod_disco:unregister_feature(Host, ?NS_PUBLISHING),
    ejabberd_hooks:delete(filter_local_packet, Host,
                          fun filter_local_packet_hook/1, 80),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_PUBLISHING),
    ets:delete(?PUBLISHING_HANDLER_TABLE),
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
    _ = handle_presence(From, To, Type, Packet),
    P;
filter_local_packet_hook(Other) ->
    Other.

%%%===================================================================
%%% Action handlers
%%%===================================================================

handle_publish(From, To, Attrs, Children) ->
    do([error_m ||
        check_same_user(From, To),
        Node    <- get_node(Attrs),
        Item    <- get_item_or_delete(Children),
        ID      <- get_id(Item#xmlel.attrs),
        Stanza  <- get_stanza(Item),
        wocky_publishing_handler:set(Node, From, To, ID, Stanza),
        {_, Version} <- wocky_publishing_handler:get(Node, From, ID),
        {ok, published_stanza(Node, ID, Version)}
       ]).

handle_items(From, To, Attrs, Children) ->
    do([error_m ||
        check_same_user(From, To),
        Node    <- get_node(Attrs),
        Param   <- get_item_id_or_rsm(Children),
        Result  <- wocky_publishing_handler:get(Node, From, Param),
        result_stanza(Result, Node)
       ]).

handle_presence(_, _, unhandled_presence_type, _) ->
    ok;

handle_presence(From, To, available, Packet) ->
    do([error_m ||
        check_same_user(From, To),
        Query <- wocky_xml:get_subel(<<"query">>, Packet),
        wocky_xml:check_namespace(?NS_PUBLISHING, Query),
        Version <- get_version(Query#xmlel.attrs),
        wocky_publishing_handler:available(To#jid.lresource, From, Version)
       ]);

handle_presence(From, _To, unavailable, _Packet) ->
    wocky_publishing_handler:unavailable(From).

%%%===================================================================
%%% Handler callbacks
%%%===================================================================

send_notification(User, Node, Item) ->
    Stanza = notification_stanza(Node, item_stanza(Item)),
    ejabberd_router:route(jid:to_bare(User), User, Stanza),
    ok.

%%%===================================================================
%%% Private helpers
%%%===================================================================

check_same_user(A, B) ->
    case jid:are_bare_equal(A, B) of
        true -> ok;
        false -> {error, ?ERR_FORBIDDEN}
    end.

get_node(Attrs) ->
    wocky_xml:get_attr(<<"node">>, Attrs).

get_version(Attrs) ->
    case xml:get_attr(<<"version">>, Attrs) of
        false -> {ok, undefined};
        {value, V} -> {ok, V}
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

published_stanza(Node, ID, Version) ->
    #xmlel{name = <<"published">>,
           attrs = [{<<"xmlns">>, ?NS_PUBLISHING},
                    {<<"node">>, Node}],
           children = [published_child(ID, Version)]}.

published_child(ID, Version) ->
    #xmlel{name = <<"item">>,
           attrs = [{<<"id">>, ID},
                    {<<"version">>, Version}]}.

result_stanza(not_found, _Node) ->
    {error, ?ERR_ITEM_NOT_FOUND};
result_stanza({#published_item{deleted = true}, _Version}, _Node) ->
    {error, ?ERR_ITEM_NOT_FOUND};
result_stanza({Item = #published_item{}, Version}, Node) ->
    items_stanza([item_stanza(Item)], Version, Node);
result_stanza({Items, Version, RSMOut}, Node) ->
    items_stanza(items_elements(Items) ++ jlib:rsm_encode(RSMOut),
                 Version, Node).

items_stanza(Children, Version, Node) ->
    {ok, #xmlel{name = <<"items">>,
                attrs = [{<<"xmlns">>, ?NS_PUBLISHING},
                         {<<"node">>, Node} |
                         maybe_version_attr(Version)
                        ],
                children = Children}}.

items_elements(Items) ->
    lists:map(item_stanza(_), Items).

item_stanza(#published_item{id = ID,
                            version = Version,
                            deleted = true}) ->
    #xmlel{name = <<"delete">>,
           attrs = [{<<"id">>, ID},
                    {<<"version">>, Version}]};
item_stanza(#published_item{id = ID,
                            version = Version,
                            from = From,
                            stanza = Stanza}) ->
    #xmlel{name = <<"item">>,
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
