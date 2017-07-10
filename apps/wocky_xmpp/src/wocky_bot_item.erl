%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module implementing Wocky bot items
%%% See https://github.com/hippware/tr-wiki/wiki/Publishing-format
%%%
-module(wocky_bot_item).

-compile({parse_transform, do}).
-compile({parse_transform, cut}).

-include("wocky.hrl").

-export([query/2,
         query_images/2,
         publish/3,
         retract/3]).


%%%===================================================================
%%% API
%%%===================================================================

query(Bot, IQ) ->
    do([error_m ||
           RSMIn <- rsm_util:get_rsm(IQ),
           {Items, RSMOut} = get_items(Bot, RSMIn),
           {ok, make_results(Items, RSMOut)}
       ]).

query_images(Bot, IQ) ->
    do([error_m ||
           RSMIn <- rsm_util:get_rsm(IQ),
           Owner = wocky_bot_util:owner_jid(Bot),
           {Images, RSMOut} = get_bot_item_images(Bot, RSMIn),
           {ok, images_result(Owner, Images, RSMOut)}
       ]).

publish(Bot, To, SubEl) ->
    do([error_m ||
           Item <- wocky_xml:get_subel(<<"item">>, SubEl),
           ItemID <- wocky_xml:get_attr(<<"id">>, Item#xmlel.attrs),
           Entry <- wocky_xml:get_subel(<<"entry">>, Item),
           wocky_xml:check_namespace(?NS_ATOM, Entry),
           publish_item(To, Bot, ItemID, Entry),
           {ok, []}
       ]).

retract(Bot, To, SubEl) ->
    do([error_m ||
           Item <- wocky_xml:get_subel(<<"item">>, SubEl),
           ItemID <- wocky_xml:get_attr(<<"id">>, Item#xmlel.attrs),
           retract_item(To, Bot, ItemID),
           {ok, []}
       ]).

%%%===================================================================
%%% Helpers - query
%%%===================================================================

get_items(Bot, RSM) ->
    ?wocky_rsm_helper:rsm_query(RSM, ?wocky_item:items_query(Bot),
                                id, {asc, updated_at}).

make_results(Items, RSMOut) ->
    #xmlel{name = <<"query">>,
           attrs = [{<<"xmlns">>, ?NS_BOT}],
           children = make_items(Items) ++ jlib:rsm_encode(RSMOut)}.

make_items(Items) ->
    [make_item_element(Item) || Item <- Items].

%%%===================================================================
%%% Helpers - query images
%%%===================================================================

get_bot_item_images(Bot, RSMIn) ->
    ?wocky_rsm_helper:rsm_query(RSMIn, ?wocky_item:images_query(Bot),
                                id, {asc, updated_at}).

images_result(Owner, Images, RSMOut) ->
    ImageEls = image_els(Owner, Images),
    #xmlel{name = <<"item_images">>,
           attrs = [{<<"xmlns">>, ?NS_BOT}],
           children = ImageEls ++ jlib:rsm_encode(RSMOut)}.

image_els(Owner, Images) ->
    lists:map(image_el(Owner, _), Images).

image_el(Owner, #{id := ID, stanza := S, updated_at := UpdatedAt}) ->
    #xmlel{name = <<"image">>,
           attrs = [{<<"owner">>, jid:to_binary(Owner)},
                    {<<"item">>, ID},
                    {<<"url">>, wocky_bot_util:get_image(S)},
                    {<<"updated">>, ?wocky_timestamp:to_string(UpdatedAt)}]}.

%%%===================================================================
%%% Helpers - publish
%%%===================================================================

publish_item(From, Bot, ItemID, Entry) ->
    Image = has_image(Entry),
    EntryBin = exml:to_binary(Entry),
    {ok, Item} = ?wocky_item:publish(Bot, ItemID, EntryBin, Image),
    Message = notification_message(Bot, make_item_element(Item)),
    notify_subscribers(From, Bot, Message).

has_image(Entry) ->
    wocky_bot_util:get_image(Entry) =/= <<>>.

%%%===================================================================
%%% Helpers - retract
%%%===================================================================

retract_item(From, Bot, ItemID) ->
    ?wocky_item:delete(Bot, ItemID),
    Message = notification_message(Bot, retract_item(ItemID)),
    notify_subscribers(From, Bot, Message).

retract_item(ItemID) ->
    #xmlel{name = <<"retract">>,
           attrs = [{<<"id">>, ItemID}]}.

%%%===================================================================
%%% Helpers - common
%%%===================================================================

make_item_element(#{id := ID, created_at := Published,
                    updated_at := Updated, stanza := Stanza}) ->
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
           [#xmlcdata{content = ?wocky_timestamp:to_string(Value)}]}.

notify_subscribers(From, Bot, Message) ->
    Subscribers = ?wocky_bot:subscribers(Bot),
    lists:foreach(notify_subscriber(From, _, Message), Subscribers).

notify_subscriber(From, To, Message) ->
    ejabberd_router:route(From, ?wocky_user:to_jid(To), Message).

notification_message(Bot, ItemEl) ->
    #xmlel{name = <<"message">>,
           attrs = [{<<"type">>, <<"headline">>}],
           children = [notification_event(Bot, ItemEl)]}.

notification_event(Bot, ItemEl) ->
    #xmlel{name = <<"event">>,
           attrs = [{<<"xmlns">>, ?NS_BOT_EVENT},
                    {<<"node">>, ?wocky_bot:make_node(Bot)}],
           children = [ItemEl]}.
