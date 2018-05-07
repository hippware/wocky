%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Behaviour module for modules providing access management
%%%
-module(wocky_access_manager).

-include("wocky.hrl").

%% Registration of access control handlers
-export([init/0, register/2, unregister/2]).

%% Access checking
-export([check_access/3, is_valid/1]).

-record(access_manager, {
          node_prefix :: binary(),
          module :: atom()
         }).

-type op() :: view | modify | delete.
-type access_result() :: allow | deny | {redirect, ejabberd:jid()}.

-export_type([op/0, access_result/0]).

-define(MANAGER_TABLE, wocky_access_managers).
%% Arbitrary but bigger than I can see us needing at the moment:
-define(MAX_REDIRECTS, 5).

-callback check_access(binary(), ejabberd:jid(), op()) -> access_result().
-callback is_access_valid(binary()) -> boolean().


%%%===================================================================
%%% Access manager registration
%%%===================================================================

-spec init() -> ok.
init() ->
    _ = ets:new(?MANAGER_TABLE, [named_table, public,
                                 {keypos, #access_manager.node_prefix}]),
    ok.

-spec register(binary(), module()) -> ok.
register(NodePrefix, Module) ->
    ets:insert(?MANAGER_TABLE, #access_manager{node_prefix = NodePrefix,
                                               module = Module}),
    ok.

-spec unregister(binary(), module()) -> ok.
unregister(NodePrefix, Module) ->
    % Under shutdown conditions, the ets table has already been deleted
    ets:info(?MANAGER_TABLE) =/= undefined
    andalso
    ets:delete_object(?MANAGER_TABLE, #access_manager{node_prefix = NodePrefix,
                                                      module = Module}),
    ok.


%%%===================================================================
%%% Access checking
%%%===================================================================

-spec check_access(ejabberd:jid(), ejabberd:jid(), op()) -> access_result().
check_access(#jid{resource = Node} = _Target, Actor, Op) ->
    case ets:lookup(?MANAGER_TABLE, node_prefix(Node)) of
        [#access_manager{module = Module}] ->
            check_access(Node, Actor, Op, Module, [Node]);

        [] ->
            deny
    end.

node_prefix(Node) ->
    hd(binary:split(Node, <<"/">>)).

check_access(Node, Actor, Op, _Module, Redirects)
  when length(Redirects) > ?MAX_REDIRECTS ->
    ok = lager:warning("Maximum redirects reached for ~p - ~p/~p",
                       [Node, Actor, Op]),
    deny;
check_access(Node, Actor, Op, Module, Redirects) ->
    case Module:check_access(Node, Actor, Op) of
        {redirect, #jid{resource = NewNode} = _Target} ->
            handle_redirect(NewNode, Actor, Op, Module, Redirects);

        Result ->
            Result
    end.

handle_redirect(Node, Actor, Op, Module, Redirects) ->
    case lists:member(Node, Redirects) of
        true ->
            ok = lager:warning("Redirect loop for ~p - ~p/~p",
                               [Node, Actor, Op]),
            deny;

        false ->
            check_access(Node, Actor, Op, Module, [Node | Redirects])
    end.

is_valid(#jid{resource = Node}) ->
    case ets:lookup(?MANAGER_TABLE, node_prefix(Node)) of
        [#access_manager{module = Module}] ->
            Module:is_access_valid(Node);
        [] ->
            false
    end.
