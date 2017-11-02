%%% @copyright 2016+ Hippware, Inc.
%%% @doc Behavior and interface to publishing service
-module(wocky_publishing_handler).

-compile({parse_transform, cut}).

-include("wocky.hrl").
-include("wocky_publishing.hrl").

-export([register/2, unregister/2, send_notification/3]).
-export([set/5, get/4, subscribe/3, unsubscribe/2]).

% Called when an item is newly published or updated
-callback publish(
            TargetJID :: ejabberd:jid(),
            UserJID   :: ejabberd:jid(),
            ItemID    :: pub_item_id(),
            Stanza    :: published_stanza()) -> ok.

% Called when a user deletes an item
-callback delete(
            UserJID :: ejabberd:jid(),
            ItemID  :: pub_item_id()) -> ok.

% Called when a user requests a specific item or set of items
-callback get(
            TargetJID      :: ejabberd:jid(),
            UserJID        :: ejabberd:jid(),
            RSM            :: jlib:rsm_in() | pub_item_id(),
            ExcludeDeleted :: boolean()) ->
    pub_get_result().

% Called when a user subscribes to a target matching the node prefix
-callback subscribe(
            TargetJID :: ejabberd:jid(),
            UserJID   :: ejabberd:jid(),
            Version   :: pub_version()) -> pub_result().

% Called when a user unsubscribes from a target matching the node prefix
-callback unsubscribe(
            UserJID :: ejabberd:jid()) -> pub_result().

%%%===================================================================
%%% Hook registration
%%%===================================================================

-spec register(binary(), module()) -> ok.
register(NodePrefix, Module) ->
    ets:insert(?PUBLISHING_HANDLER_TABLE, {NodePrefix, Module}),
    ok.

-spec unregister(binary(), module()) -> ok.
unregister(NodePrefix, Module) ->
    % Under shutdown conditions, the ets table has already been deleted
    ets:info(?PUBLISHING_HANDLER_TABLE) =/= undefined
    andalso
    ets:delete_object(?PUBLISHING_HANDLER_TABLE, {NodePrefix, Module}),
    ok.

%%%===================================================================
%%% Handler API
%%%===================================================================

-spec send_notification(ejabberd:jid(), pub_node(), pub_item()) -> ok.
send_notification(User, Node, Item) ->
    mod_wocky_publishing:send_notification(User, Node, Item).

%%%===================================================================
%%% Hook calls
%%%===================================================================

set(TargetJID, _From, To, ID, #xmlel{name = <<"delete">>}) ->
    call_hook(delete, TargetJID, [To, ID]);
set(TargetJID, From, To, ID, Stanza) ->
    call_hook(publish, TargetJID, [To, From, ID, Stanza]).

get(TargetJID, From, Param, ExcludeDeleted) ->
    call_hook(get, TargetJID, [TargetJID, From, Param, ExcludeDeleted]).

-spec subscribe(ejabberd:jid(), ejabberd:jid(), pub_version()) -> pub_result().
subscribe(TargetJID, UserJID, Version) ->
    call_hook(subscribe, TargetJID, [TargetJID, UserJID, Version]).

-spec unsubscribe(all | ejabberd:jid(), ejabberd:jid()) -> pub_result().
unsubscribe(all, UserJID) ->
    lists:foreach(
      call_hook(unsubscribe, _, [UserJID]),
      all_nodes(UserJID));

unsubscribe(TargetJID, UserJID) ->
    call_hook(unsubscribe, TargetJID, [UserJID]).

%%%===================================================================
%%% Helpers
%%%===================================================================

call_hook(Hook, TargetJID, Args) ->
    case ets:lookup(?PUBLISHING_HANDLER_TABLE, node_prefix(TargetJID)) of
        [{_, HandlerMod}] ->
            apply(HandlerMod, Hook, Args);
        [] ->
            {error, ?ERRT_SERVICE_UNAVAILABLE(?MYLANG, <<"Unknown node type">>)}
    end.

all_nodes(User) ->
    [jid:replace_resource(User, NodePrefix)
     || {NodePrefix, _} <- ets:tab2list(?PUBLISHING_HANDLER_TABLE)].

node_prefix(#jid{lresource = LResource}) ->
    hd(binary:split(LResource, <<"/">>)).
