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
         publish/4,
         retract/4]).


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


-spec publish(?wocky_bot:t(), ?wocky_user:t(), jlib:jid(), jlib:xmlel()) ->
{ok, []} | {error, binary()}.
publish(Bot, From, ToJID, SubEl) ->
    do([error_m ||
           Item <- wocky_xml:get_subel(<<"item">>, SubEl),
           ItemID <- wocky_xml:get_attr(<<"id">>, Item#xmlel.attrs),
           Entry <- wocky_xml:get_subel(<<"entry">>, Item),
           wocky_xml:check_namespace(?NS_ATOM, Entry),
           check_can_publish(From, Bot, ItemID),
           publish_item(From, ToJID, Bot, ItemID, Entry),
           {ok, []}
       ]).

retract(Bot, From, ToJID, SubEl) ->
    do([error_m ||
           Item <- wocky_xml:get_subel(<<"item">>, SubEl),
           ItemID <- wocky_xml:get_attr(<<"id">>, Item#xmlel.attrs),
           check_can_retract(From, Bot, ItemID),
           retract_item(From, ToJID, Bot, ItemID),
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

publish_item(From, ToJID, Bot, ItemID, Entry) ->
    Image = has_image(Entry),
    EntryBin = exml:to_binary(Entry),
    {ok, Item} = ?wocky_item:publish(Bot, From, ItemID, EntryBin, Image),
    Message = notification_message(Bot, make_item_element(Item)),
    notify_subscribers(From, ToJID, Bot, Message).

has_image(Entry) ->
    wocky_bot_util:get_image(Entry) =/= <<>>.

check_can_publish(#{id := UserID}, Bot, ItemID) ->
    case ?wocky_item:get(Bot, ItemID) of
        nil -> ok;
        #{user_id := UserID} -> ok;
        _ -> {error, ?ERR_FORBIDDEN}
    end.

%%%===================================================================
%%% Helpers - retract
%%%===================================================================

retract_item(From, ToJID, Bot, ItemID) ->
    ?wocky_item:delete(Bot, ItemID),
    Message = notification_message(Bot, retract_item(ItemID)),
    notify_subscribers(From, ToJID, Bot, Message).

retract_item(ItemID) ->
    #xmlel{name = <<"retract">>,
           attrs = [{<<"id">>, ItemID}]}.

check_can_retract(#{id := UserID}, Bot = #{user_id := BotOwner}, ItemID) ->
    case ?wocky_item:get(Bot, ItemID) of
        nil -> {error, ?ERR_ITEM_NOT_FOUND};
        #{user_id := UserID} -> ok;
        _ when UserID =:= BotOwner -> ok;
        _ -> {error, ?ERR_FORBIDDEN}
    end.

%%%===================================================================
%%% Helpers - common
%%%===================================================================

make_item_element(I = #{id := ID, created_at := Published,
                        updated_at := Updated, stanza := Stanza}) ->
    Item = ?wocky_repo:preload(I, user),
    User = maps:get(user, Item),
    {ok, Entry} = exml:parse(Stanza),
    FullEntry = add_time_fields(Published, Updated, Entry),

    BaseAttrs = [{<<"id">>, ID},
                 {<<"author">>, jid:to_binary(?wocky_user:to_jid(User))},
                 {<<"author_handle">>, maps:get(handle, User)},
                 {<<"author_avatar">>, maps:get(avatar, User)},
                 {<<"author_first_name">>, maps:get(first_name, User)},
                 {<<"author_last_name">>, maps:get(last_name, User)}
                ],
    Attrs = [{K, wocky_util:nil_to_bin(V)} || {K, V} <- BaseAttrs],

    #xmlel{name = <<"item">>,
           attrs = Attrs,
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

notify_subscribers(From, ToJID, Bot, Message) ->
    NotificationJIDs = ?wocky_bot:notification_recipients(Bot, From),
    lists:foreach(notify_subscriber(ToJID, _, Message), NotificationJIDs).

notify_subscriber(FromJID, ToJID, Message) ->
    ejabberd_router:route(FromJID, ToJID, Message).

notification_message(Bot, ItemEl) ->
    #xmlel{name = <<"message">>,
           attrs = [{<<"type">>, <<"headline">>}],
           children = [notification_event(Bot, ItemEl)]}.

notification_event(Bot, ItemEl) ->
    #xmlel{name = <<"event">>,
           attrs = [{<<"xmlns">>, ?NS_BOT_EVENT},
                    {<<"node">>, ?wocky_bot:make_node(Bot)}],
           children = [ItemEl]}.
