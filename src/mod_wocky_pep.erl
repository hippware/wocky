%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module implementing Personal Eventing Protocol (XEP-0163)
%%% See http://xmpp.org/extensions/xep-0163.html
%%%
-module(mod_wocky_pep).

-behaviour(gen_mod).

-compile({parse_transform, do}).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").
-include("wocky_roster.hrl").

-define(HOOK_TABLE, mod_wocky_pep_hooks).

-ignore_xref([{handle_iq, 3}]).

%% gen_mod handlers
-export([start/2, stop/1]).

%% IQ hook
-export([handle_iq/3]).

%% Event type processing
-export([register_handler/2, unregister_handler/2]).


%%%===================================================================
%%% gen_mod handlers
%%%===================================================================

start(Host, _Opts) ->
    _ = ets:new(?HOOK_TABLE, [named_table, public, {read_concurrency, true}]),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_PUBSUB,
                                  ?MODULE, handle_iq, parallel),
    mod_disco:register_feature(Host, ?NS_PUBSUB).

stop(Host) ->
    mod_disco:unregister_feature(Host, ?NS_PUBSUB),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_PUBSUB).


%%%===================================================================
%%% Hook registration
%%%===================================================================

-spec register_handler(binary(), module()) -> ok.
register_handler(Namespace, Module) ->
    ets:insert(?HOOK_TABLE, {Namespace, Module}),
    ok.

-spec unregister_handler(binary(), module()) -> ok.
unregister_handler(Namespace, Module) ->
    ets:delete_object(?HOOK_TABLE, {Namespace, Module}),
    ok.


%%%===================================================================
%%% Event handler
%%%===================================================================

-spec handle_iq(From :: ejabberd:jid(),
                To :: ejabberd:jid(),
                IQ :: iq()) -> iq().
handle_iq(From, _To, IQ) ->
    case handle_iq_type(From, IQ) of
        {ok, ResponseIQ} -> ResponseIQ;
        {error, Error} -> wocky_util:make_error_iq_response(IQ, Error)
    end.

handle_iq_type(From, IQ = #iq{type = Type,
                              sub_el = #xmlel{name = <<"pubsub">>,
                                              children = Children}
                             }) ->
    handle_pubsub(From, IQ, Type, Children);
handle_iq_type(_From, _IQ) ->
    {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Invalid query">>)}.

handle_pubsub(From, IQ, set, [#xmlel{name = <<"publish">>,
                                     attrs = Attrs,
                                     children = Children}]) ->
    publish(From, IQ, Attrs, Children);
handle_pubsub(From, IQ, set, [_ | Rest]) ->
    handle_pubsub(From, IQ, set, Rest);
handle_pubsub(_, _, _, []) ->
    {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Invalid request type">>)}.


%%%===================================================================
%%% Actions - publish
%%%===================================================================

publish(From, IQ, Attrs, Children) ->
    do([error_m ||
        Node <- check_node(Attrs),
        Items <- extract_items(Children),
        Items2 <- maybe_mutate_items(From, Items),
        forward_items(From, Node, Items2),
        publish_response(IQ, Node)
       ]).

check_node(Attrs) ->
    case xml:get_attr(<<"node">>, Attrs) of
        false -> {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Missing node">>)};
        {value, Value} -> {ok, Value}
    end.

extract_items(Children) ->
    {ok, lists:filter(fun(#xmlel{name = <<"item">>}) -> true;
                         (_) -> false
                      end,
                      Children)}.

maybe_mutate_items(From, Items) ->
    MutatedItems = lists:map(fun(I) -> maybe_mutate_item(From, I) end, Items),
    {ok, lists:filter(fun(I) -> I =/= undefined end, MutatedItems)}.

maybe_mutate_item(From, Item = #xmlel{children = Elements}) ->
    Elements2 = maybe_mutate_elements(From, Elements),
    case Elements2 of
        [] -> undefined;
        _ -> Item#xmlel{children = Elements2}
    end.

maybe_mutate_elements(From, Elements) ->
    MutatedElements = lists:map(fun(E) ->
                                        maybe_mutate_element(From, E)
                                end,
                                Elements),
    lists:filter(fun(I) -> I =/= undefined end, MutatedElements).

maybe_mutate_element(From, Element = #xmlel{attrs = Attrs}) ->
    case xml:get_attr(<<"xmlns">>, Attrs) of
        false -> Element;
        {value, NS} -> maybe_mutate_element(From, Element, NS)
    end.

maybe_mutate_element(From, Element, NS) ->
    case ets:lookup(?HOOK_TABLE, NS) of
        [] -> Element;
        [{NS, Module}] -> Module:handle_pep(From, Element)
    end.

forward_items(_From, _Node, []) ->
    ok;
forward_items(From = #jid{luser = LUser, lserver = LServer}, Node, Items) ->
    Roster = wocky_db_roster:get_roster(LUser, LServer),
    PresenceSubs = get_presence_subs(Roster),
    Packet = make_forward_message(Items, Node),
    lists:foreach(fun(S) -> forward_to_user(From, Packet, S) end,
                  [jid:to_bare(From) | PresenceSubs]).

get_presence_subs(Roster) ->
    [jid:make(R#wocky_roster.contact_jid)
     || R <- Roster,
        R#wocky_roster.subscription =:= both orelse
        R#wocky_roster.subscription =:= to].

make_forward_message(Items, Node) ->
    #xmlel{name = <<"message">>,
           attrs = [{<<"type">>, <<"headline">>}],
           children = [make_event(Items, Node)]}.

make_event(Items, Node) ->
    #xmlel{name = <<"event">>,
           attrs = [{<<"xmlns">>, ?NS_PUBSUB_EVENT}],
           children = [#xmlel{name = <<"items">>,
                              attrs = [{<<"node">>, Node}],
                              children = Items}]}.

forward_to_user(From, Packet, User) ->
    ejabberd_router:route(From, User, Packet).

publish_response(IQ, Node) ->
    {ok, IQ#iq{type = result,
               sub_el = [#xmlel{name = <<"pubsub">>,
                                attrs = [{<<"xmlns">>, ?NS_PUBSUB}],
                                children = [#xmlel{name = <<"publish">>,
                                                   attrs = [{<<"node">>, Node}]
                                                  }]}]}}.
