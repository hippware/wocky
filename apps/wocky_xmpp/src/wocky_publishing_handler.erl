%%% @copyright 2016+ Hippware, Inc.
%%% @doc Behavior and interface to publishing service
-module(wocky_publishing_handler).

-compile({parse_transform, cut}).

-include("wocky.hrl").
-include("wocky_publishing.hrl").

-export([register/2, unregister/2, send_notification/3]).
-export([set/5, get/4, subscribe/3, unsubscribe/2]).

-callback publish(ejabberd:jid(), ejabberd:jid(), pub_item_id(),
              published_stanza()) -> ok.

-callback delete(ejabberd:jid(), pub_item_id()) -> ok.

-callback get(ejabberd:jid(), jlib:rsm_in() | pub_item_id(), boolean()) ->
    {ok, {[published_item()], pub_version(), jlib:rsm_out()} |
         {published_item(), pub_version()} |
         not_found} |
    {error, term()}.

-callback subscribe(ejabberd:jid(), pub_version()) -> ok.

-callback unsubscribe(ejabberd:jid()) -> ok.


%%%===================================================================
%%% Hook registration
%%%===================================================================

-spec register(binary(), module()) -> ok.
register(Node, Module) ->
    ets:insert(?PUBLISHING_HANDLER_TABLE, {Node, Module}),
    ok.

-spec unregister(binary(), module()) -> ok.
unregister(Node, Module) ->
    % Under shutdown conditions, the ets table has already been deleted
    ets:info(?PUBLISHING_HANDLER_TABLE) =/= undefined
    andalso
    ets:delete_object(?PUBLISHING_HANDLER_TABLE, {Node, Module}),
    ok.

%%%===================================================================
%%% Handler API
%%%===================================================================

-spec send_notification(ejabberd:jid(), publishing_node(), published_item()) ->
    ok.
send_notification(User, Node, Item) ->
    mod_wocky_publishing:send_notification(User, Node, Item).

%%%===================================================================
%%% Hook calls
%%%===================================================================

set(Node, _From, To, ID, #xmlel{name = <<"delete">>}) ->
    call_hook(delete, Node, [To, ID]);
set(Node, From, To, ID, Stanza) ->
    call_hook(publish, Node, [To, From, ID, Stanza]).

get(Node, From, Param, ExcludeDeleted) ->
    call_hook(get, Node, [From, Param, ExcludeDeleted]).

subscribe(Node, User, Version) ->
    call_hook(subscribe, Node, [User, Version]).

unsubscribe(all, User) ->
    lists:foreach(
      call_hook(unsubscribe, _, [User]),
      all_nodes());

unsubscribe(Node, User) ->
    call_hook(unsubscribe, Node, [User]).

%%%===================================================================
%%% Helpers
%%%===================================================================

call_hook(Hook, Node, Args) ->
    case ets:lookup(?PUBLISHING_HANDLER_TABLE, Node) of
        [{Node, HandlerMod}] ->
            apply(HandlerMod, Hook, Args);
        [] ->
            {error, ?ERRT_SERVICE_UNAVAILABLE(?MYLANG, <<"Unknown node type">>)}
    end.

all_nodes() ->
    [HandlerMod || {HandlerMod, _} <- ets:tab2list(?PUBLISHING_HANDLER_TABLE)].
