%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module implementing Wocky bot items
%%% See https://github.com/hippware/tr-wiki/wiki/Publishing-format
%%%
-module(wocky_bot_item).

-compile({parse_transform, do}).
-compile({parse_transform, cut}).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").
-include("wocky.hrl").

-export([handle_query/4,
         handle_publish/4,
         handle_retract/4]).

%%%===================================================================
%%% API
%%%===================================================================

handle_query(From, #jid{lserver = LServer}, IQ, Attrs) ->
    do([error_m ||
        BotID <- wocky_bot_util:get_id_from_node(Attrs),
        wocky_bot_util:check_access(LServer, BotID, From),
        RSMIn <- rsm_util:get_rsm(IQ),
        {Items, RSMOut} <- get_items(LServer, BotID, RSMIn),
        {ok, make_results(Items, RSMOut)}
       ]).

handle_retract(From, To = #jid{lserver = LServer}, SubEl, Attrs) ->
    do([error_m ||
        BotID <- wocky_bot_util:get_id_from_node(Attrs),
        wocky_bot_util:check_owner(LServer, BotID, From),
        Item <- wocky_xml:get_sub_el(<<"item">>, SubEl),
        ItemID <- wocky_xml:get_attr(<<"id">>, Item#xmlel.attrs),
        retract_item(To, BotID, ItemID),
        {ok, []}
       ]).

handle_publish(From, To = #jid{lserver = LServer}, SubEl, Attrs) ->
    do([error_m ||
        BotID <- wocky_bot_util:get_id_from_node(Attrs),
        wocky_bot_util:check_owner(LServer, BotID, From),
        Item <- wocky_xml:get_sub_el(<<"item">>, SubEl),
        ItemID <- wocky_xml:get_attr(<<"id">>, Item#xmlel.attrs),
        Entry <- wocky_xml:get_sub_el(<<"entry">>, Item),
        wocky_xml:check_namespace(?NS_ATOM, Entry),
        publish_item(To, BotID, ItemID, Entry),
        {ok, []}
       ]).

%%%===================================================================
%%% Helpers - query
%%%===================================================================

get_items(LServer, BotID, RSM) ->
    Items = wocky_db_bot:get_items(LServer, BotID),
    SortedItems = lists:sort(updated_order(_, _), Items),
    {ok, rsm_util:filter_with_rsm(SortedItems, RSM)}.

updated_order(#{updated := U1}, #{updated := U2}) ->
    U1 =< U2.

make_results(Items, RSMOut) ->
    #xmlel{name = <<"query">>,
           attrs = [{<<"xmlns">>, ?NS_BOT}],
           children =
           make_items(Items) ++
           jlib:rsm_encode(RSMOut)
          }.

make_items(Items) ->
    [make_item_element(Item) || Item <- Items].

%%%===================================================================
%%% Helpers - retract
%%%===================================================================

retract_item(From = #jid{lserver = LServer}, BotID, ItemID) ->
    wocky_db_bot:delete_item(LServer, BotID, ItemID),
    Message = notification_message(BotID, retract_item(ItemID)),
    notify_subscribers(From, BotID, Message).

retract_item(ItemID) ->
    #xmlel{name = <<"retract">>,
           attrs = [{<<"id">>, ItemID}]}.

%%%===================================================================
%%% Helpers - publish
%%%===================================================================

publish_item(From = #jid{lserver = LServer}, BotID, ItemID, Entry) ->
    Image = has_image(Entry),
    EntryBin = exml:to_binary(Entry),
    wocky_db_bot:publish_item(LServer, BotID, ItemID, EntryBin, Image),
    Item = wocky_db_bot:get_item(LServer, BotID, ItemID),
    Message = notification_message(BotID, make_item_element(Item)),
    notify_subscribers(From, BotID, Message).

notify_subscribers(From = #jid{lserver = LServer}, BotID, Message) ->
    Subscribers = wocky_db_bot:subscribers(LServer, BotID),
    {SubscriberJIDs, _FollowStatuses} = lists:unzip(Subscribers),
    lists:foreach(notify_subscriber(From, _, Message), SubscriberJIDs).

has_image(Entry) ->
    wocky_bot_util:get_image(Entry) =/= none.

%%%===================================================================
%%% Helpers - common
%%%===================================================================

make_item_element(#{id := ID, published := Published,
                    updated := Updated, stanza := Stanza}) ->
    {ok, Entry} = exml:parse(Stanza),
    FullEntry = add_time_fields(Published, Updated, Entry),
    #xmlel{name = <<"item">>,
           attrs = [{<<"id">>, ID}],
           children = [FullEntry]}.

add_time_fields(Published, Updated, Entry = #xmlel{children = Children}) ->
    NonTimeChildren = lists:filter(fun(El) -> not is_time_el(El) end, Children),
    NewChildren = [time_field(<<"published">>, Published),
                   time_field(<<"updated">>, Updated)
                   | NonTimeChildren],
    Entry#xmlel{children = NewChildren}.

is_time_el(#xmlel{name = <<"published">>}) -> true;
is_time_el(#xmlel{name = <<"updated">>}) -> true;
is_time_el(_) -> false.

time_field(Name, Value) ->
    #xmlel{name = Name,
           children =
           [#xmlcdata{content = wocky_db:timestamp_to_string(Value)}]}.

notify_subscriber(From, To, Message) ->
    ejabberd_router:route(From, To, Message).

notification_message(BotID, ItemEl) ->
    #xmlel{name = <<"message">>,
           attrs = [{<<"type">>, <<"headline">>}],
           children = [notification_event(BotID, ItemEl)]}.

notification_event(BotID, ItemEl) ->
    #xmlel{name = <<"event">>,
           attrs = [{<<"xmlns">>, ?NS_BOT_EVENT},
                    {<<"node">>, wocky_bot_util:make_node(BotID)}],
           children = [ItemEl]}.
