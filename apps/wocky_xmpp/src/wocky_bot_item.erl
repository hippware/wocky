%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module implementing Wocky bot items
%%% See https://github.com/hippware/tr-wiki/wiki/Publishing-format
%%%
-module(wocky_bot_item).

-compile({parse_transform, do}).
-compile({parse_transform, cut}).

-include("wocky.hrl").

-export([query/3,
         query_images/3,
         publish/4,
         retract/4]).


%%%===================================================================
%%% API
%%%===================================================================

query(Bot, IQ, FromID) ->
    do([error_m ||
           RSMIn <- rsm_util:get_rsm(IQ),
           {Items, RSMOut} = get_items(Bot, RSMIn, FromID),
           {ok, make_results(Items, RSMOut)}
       ]).

query_images(Bot, IQ, FromUser) ->
    do([error_m ||
           RSMIn <- rsm_util:get_rsm(IQ),
           Owner = wocky_bot_util:owner_jid(Bot),
           {Images, RSMOut} = get_bot_item_images(Bot, FromUser, RSMIn),
           {ok, images_result(Owner, FromUser, Images, RSMOut)}
       ]).


-spec publish(?wocky_bot:t(), ?wocky_user:t(), ejabberd:jid(), jlib:xmlel()) ->
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

retract(Bot, From, _ToJID, SubEl) ->
    do([error_m ||
           Item <- wocky_xml:get_subel(<<"item">>, SubEl),
           ItemID <- wocky_xml:get_attr(<<"id">>, Item#xmlel.attrs),
           check_can_retract(From, Bot, ItemID),
           ?wocky_item:delete(Bot, ItemID),
           {ok, []}
       ]).

%%%===================================================================
%%% Helpers - query
%%%===================================================================

get_items(Bot, RSM, FromID) ->
    Query = ?wocky_block:object_visible_query(
               ?wocky_item:items_query(Bot), FromID, user_id),
    ?wocky_rsm_helper:rsm_query(RSM, Query, id, {asc, updated_at}).

make_results(Items, RSMOut) ->
    #xmlel{name = <<"query">>,
           attrs = [{<<"xmlns">>, ?NS_BOT}],
           children = make_items(Items) ++ jlib:rsm_encode(RSMOut)}.

make_items(Items) ->
    [make_item_element(Item) || Item <- Items].

%%%===================================================================
%%% Helpers - query images
%%%===================================================================

get_bot_item_images(Bot, #{id := FromID}, RSMIn) ->
    Query = ?wocky_block:object_visible_query(
               ?wocky_item:images_query(Bot), FromID, user_id),
    ?wocky_rsm_helper:rsm_query(RSMIn, Query, id, {asc, updated_at}).

images_result(Owner, FromUser, Images, RSMOut) ->
    ImageEls = image_els(Owner, FromUser, Images),
    #xmlel{name = <<"item_images">>,
           attrs = [{<<"xmlns">>, ?NS_BOT}],
           children = ImageEls ++ jlib:rsm_encode(RSMOut)}.

image_els(Owner, FromUser, Images) ->
    FromJID = ?wocky_user:to_jid(FromUser),
    lists:map(image_el(Owner, FromJID, _), Images).

image_el(Owner, FromJID, #{id := ID, stanza := S, updated_at := UpdatedAt}) ->
    TROSURL = ?wocky_item:get_image(S),
    {Full, Thumbnail} = mod_wocky_tros:get_download_urls(TROSURL, FromJID),
    #xmlel{name = <<"image">>,
           attrs = [{<<"owner">>, jid:to_binary(Owner)},
                    {<<"item">>, ID},
                    {<<"url">>, TROSURL},
                    {<<"full_url">>, Full},
                    {<<"thumbnail_url">>, Thumbnail},
                    {<<"updated">>, ?wocky_timestamp:to_string(UpdatedAt)}]}.

%%%===================================================================
%%% Helpers - publish
%%%===================================================================

publish_item(From, ToJID, Bot, ItemID, Entry) ->
    EntryBin = exml:to_binary(Entry),
    {ok, Item} = ?wocky_item:publish(Bot, From, ItemID, EntryBin),
    Message = notification_event(Bot, make_item_element(Item)),
    wocky_bot_users:notify_subscribers_and_watchers(Bot, From, ToJID, Message).

check_can_publish(#{id := UserID}, Bot, ItemID) ->
    case ?wocky_item:get(Bot, ItemID) of
        nil -> ok;
        #{user_id := UserID} -> ok;
        _ -> {error, ?ERR_FORBIDDEN}
    end.

%%%===================================================================
%%% Helpers - retract
%%%===================================================================

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

notification_event(Bot, ItemEl) ->
    #xmlel{name = <<"event">>,
           attrs = [{<<"xmlns">>, ?NS_BOT_EVENT},
                    {<<"node">>, ?wocky_bot:make_node(Bot)}],
           children = [ItemEl]}.
