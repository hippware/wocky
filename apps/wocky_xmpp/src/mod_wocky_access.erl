%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module implementing Wocky access control
%%% See https://github.com/hippware/tr-wiki/wiki/Access-Control-Query
%%%
%%% NOTE: This module must be started *before* any modules that attempt to
%%% register with it has access managers.
%%%
%%% Access manager modules - that is, those providing access query
%%% functionality for particular node types, must implement the
%%% wocky_access_manager behaviour.
%%%
-module(mod_wocky_access).

-compile({parse_transform, do}).

-include("wocky.hrl").

-behaviour(gen_mod).

-export([init/0]).

%% gen_mod handlers
-export([start/2, stop/1]).

%% IQ handler
-export([handle_iq/3]).

%% Registration of access control handlers
-export([register/2, unregister/2]).

-record(access_manager, {
          node_prefix :: binary(),
          module :: atom()
         }).

-type op() :: view | modify | delete.
-type access_result() :: allow | deny | {redirect, ejabberd:jid()}.

-export_type([op/0, access_result/0]).

-define(MANAGER_TABLE, mod_wocky_access_managers).


%%%===================================================================
%%% gen_mod handlers
%%%===================================================================

init() ->
    _ = ets:new(?MANAGER_TABLE, [named_table, public,
                                 {keypos, #access_manager.node_prefix}]),
    ok.

start(Host, _Opts) ->
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_ACCESS,
                                  ?MODULE, handle_iq, parallel),
    mod_disco:register_feature(Host, ?NS_ACCESS).

stop(Host) ->
    mod_disco:unregister_feature(Host, ?NS_ACCESS),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_ACCESS),
    ets:delete(?MANAGER_TABLE).


%%%===================================================================
%%% Event handler
%%%===================================================================

-spec handle_iq(From :: ejabberd:jid(),
                To :: ejabberd:jid(),
                IQ :: iq()) -> iq().
handle_iq(From, To, IQ) ->
    case handle_iq_type(From, To, IQ) of
        {ok, SubEl} -> IQ#iq{type = result, sub_el = SubEl};
        {error, Error} -> wocky_util:make_error_iq_response(IQ, Error)
    end.

% Query
handle_iq_type(_From, _To, #iq{type = get,
                               sub_el = #xmlel{name = <<"query">>,
                                               attrs = Attrs}}) ->
    do([error_m ||
        Node <- wocky_xml:get_attr(<<"node">>, Attrs),
        Actor <- wocky_xml:get_attr(<<"actor">>, Attrs),
        OpBin <- wocky_xml:get_attr(<<"op">>, Attrs),
        Op <- check_op(OpBin),
        {ok, check_access(Node, Actor, Op)}
       ]).

check_access(Node, Actor, Op) ->
    Result =
    case ets:lookup(?MANAGER_TABLE, node_prefix(Node)) of
        [#access_manager{module = Module}] ->
            wocky_access_manager:check_access(Node, Actor, Op, Module);
        [] ->
            deny
    end,
    access_result(Result).

check_op(<<"view">>) -> {ok, view};
check_op(<<"modify">>) -> {ok, modify};
check_op(<<"delete">>) -> {ok, delete};
check_op(_) -> {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Invalid operation">>)}.

node_prefix(Node) ->
    hd(binary:split(Node, <<"/">>)).

access_result(allow) ->
    #xmlel{name = <<"allow">>};
access_result(deny) ->
    #xmlel{name = <<"deny">>};
access_result({redirect, Target}) ->
    #xmlel{name = <<"redirect">>,
           children = [#xmlcdata{content = jid:to_binary(Target)}]}.

%%%===================================================================
%%% Access manager registration
%%%===================================================================

-spec register(binary(), module()) -> ok.
register(NodePrefix, Module) ->
    ets:insert(?MANAGER_TABLE, #access_manager{node_prefix = NodePrefix,
                                               module = Module}),
    ok.

-spec unregister(binary(), module()) -> ok.
unregister(NodePrefix, Module) ->
    ets:delete_object(?MANAGER_TABLE, #access_manager{node_prefix = NodePrefix,
                                                      module = Module}),
    ok.
