%%% @copyright 2016+ Hippware, Inc.
%%% @doc Behavior and interface to publishing service
-module(wocky_publishing_handler).

-include_lib("wocky_publishing.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").

-compile({parse_transform, cut}).

-export([register/2, unregister/2, send_notification/3]).

-export([set/5, get/3, available/3, unavailable/1]).

-callback publish(ejabberd:jid(), ejabberd:jid(), pub_item_id(),
              published_stanza()) -> ok.

-callback delete(ejabberd:jid(), pub_item_id()) -> ok.

-callback get(ejabberd:jid(), jlib:rsm_in() | pub_item_id()) ->
    {ok, {[published_item()], pub_version(), jlib:rsm_out()} |
         {published_item(), pub_version()} |
         not_found} |
    {error, term()}.

-callback available(ejabberd:jid(), pub_version()) -> ok.

-callback unavailable(ejabberd:jid()) -> ok.

%%%===================================================================
%%% Hook registration
%%%===================================================================

-spec register(binary(), module()) -> ok.
register(Node, Module) ->
    ets:insert(?PUBLISHING_HANDLER_TABLE, {Node, Module}),
    ok.

-spec unregister(binary(), module()) -> ok.
unregister(Node, Module) ->
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

get(Node, From, Param) ->
    call_hook(get, Node, [From, Param]).

available(Node, User, Version) ->
    call_hook(available, Node, [User, Version]).

unavailable(User) ->
    lists:foreach(
      call_hook(unavailable, _, [User]),
      all_nodes()).

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
