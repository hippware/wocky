%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module implementing Personal Eventing Protocol (XEP-0163)
%%% See http://xmpp.org/extensions/xep-0163.html
%%%
-module(mod_wocky_pep).

-behaviour(gen_mod).

-compile({parse_transform, do}).
-compile({parse_transform, cut}).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").
-include("wocky_roster.hrl").

-define(HANDLER_TABLE, mod_wocky_pep_handlers).

-ignore_xref([{handle_iq, 3}]).

%% gen_mod handlers
-export([start/2, stop/1]).

%% IQ hook
-export([handle_iq/3]).

%% Event type processing
-export([register_handler/3, unregister_handler/3]).

-type pep_model() :: open | presence | roster | whitelist.

-export_type([pep_model/0]).

%%%===================================================================
%%% gen_mod handlers
%%%===================================================================

start(Host, _Opts) ->
    _ = ets:new(?HANDLER_TABLE,
                [named_table, public, {read_concurrency, true}]),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_PUBSUB,
                                  ?MODULE, handle_iq, parallel),
    mod_disco:register_feature(Host, ?NS_PUBSUB).

stop(Host) ->
    mod_disco:unregister_feature(Host, ?NS_PUBSUB),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_PUBSUB).


%%%===================================================================
%%% Hook registration
%%%===================================================================

-spec register_handler(binary(), pep_model(), module()) -> ok.
register_handler(Namespace, Model, Module) ->
    ets:insert(?HANDLER_TABLE, {Namespace, Model, Module}),
    ok.

-spec unregister_handler(binary(), pep_model(), module()) -> ok.
unregister_handler(Namespace, Model, Module) ->
    ets:delete_object(?HANDLER_TABLE, {Namespace, Model, Module}),
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
        {Model, Mutator} <- get_model_mutator(Node),
        Items2 <- maybe_mutate_items(From, Mutator, Items),
        forward_items(From, Model, Node, Items2),
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

get_model_mutator(Node) ->
    R = case ets:lookup(?HANDLER_TABLE, Node) of
        [] ->
            {presence, fun(_From, I) -> I end};
        [{Node, Model, HandlerMod}] ->
            {Model, fun HandlerMod:handle_pep/2}
    end,
    {ok, R}.

maybe_mutate_items(From, Mutator, Items) ->
    MutatedItems = lists:foldl(mutate_item(From, Mutator, _, _), [], Items),
    {ok, lists:reverse(MutatedItems)}.

mutate_item(From, Mutator, Item = #xmlel{children = Elements}, Acc) ->
    case mutate_elements(From, Mutator, Elements) of
        [] -> Acc;
        Elements -> [Item#xmlel{children = Elements} | Acc]
    end.

mutate_elements(From, Mutator, Elements) ->
    lists:reverse(
      lists:foldl(mutate_element(From, Mutator, _, _), [], Elements)).

mutate_element(From, Mutator, Element, Acc) ->
    case Mutator(From, Element) of
        drop -> Acc;
        Mutated -> [Mutated | Acc]
    end.

forward_items(From, Model, Node, Items) ->
    lists:foreach(forward_item(Model, From, Node, _), Items).

forward_item(open, _From, _Node, _Item) ->
    erlang:error(unimplemented);
forward_item(presence, From = #jid{luser = LUser, lserver = LServer},
             Node, Item) ->
    Roster = wocky_db_roster:get_roster(LUser, LServer),
    PresenceSubs = get_presence_subs(Roster),
    lists:foreach(forward_to_user(From, _, Node, Item),
                  add_self(From, PresenceSubs));
forward_item(roster, From = #jid{luser = LUser, lserver = LServer},
             Node, Item) ->
    Roster = wocky_db_roster:get_roster(LUser, LServer),
    Contacts = get_non_blocked(Roster),
    lists:foreach(forward_to_user(From, _, Node, Item),
                  add_self(From, Contacts));
forward_item(whitelist, From, Node, Item) ->
    % TODO Implement whitelist subscription system if/when required
    WhitelistedSubs = [],
    lists:foreach(forward_to_user(From, _, Node, Item),
                  add_self(From, WhitelistedSubs)).

add_self(Self, Others) ->
    [jid:to_bare(Self) | Others].

get_presence_subs(Roster) ->
    [jid:make(R#wocky_roster.contact_jid)
     || R <- Roster,
        R#wocky_roster.subscription =:= both orelse
        R#wocky_roster.subscription =:= to].

get_non_blocked(Roster) ->
    [jid:make(R#wocky_roster.contact_jid)
     || R <- Roster,
        not lists:member(<<"__blocked__">>, R#wocky_roster.groups)].

make_forward_message(Item, Node) ->
    #xmlel{name = <<"message">>,
           attrs = [{<<"type">>, <<"headline">>}],
           children = [make_event(Item, Node)]}.

make_event(Item, Node) ->
    #xmlel{name = <<"event">>,
           attrs = [{<<"xmlns">>, ?NS_PUBSUB_EVENT}],
           children = [#xmlel{name = <<"items">>,
                              attrs = [{<<"node">>, Node}],
                              children = [Item]}]}.

forward_to_user(From, User, Node, Item) ->
    Packet = make_forward_message(Item, Node),
    ejabberd_router:route(From, User, Packet).

publish_response(IQ, Node) ->
    {ok, IQ#iq{type = result,
               sub_el = [#xmlel{name = <<"pubsub">>,
                                attrs = [{<<"xmlns">>, ?NS_PUBSUB}],
                                children = [#xmlel{name = <<"publish">>,
                                                   attrs = [{<<"node">>, Node}]
                                                  }]}]}}.
