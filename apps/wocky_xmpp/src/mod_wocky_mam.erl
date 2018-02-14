%%% @copyright 2016+ Hippware, Inc.
%%% @doc RSM <reverse> tag support for MAM
%%%
%%% This module hooks into the MAM message lookup chain and applies
%%% the <reverse> RSM tag if it's present.

-module(mod_wocky_mam).

-include("wocky.hrl").

-behaviour(gen_mod).

%% gen_mod handlers
-export([start/2, stop/1]).
%% MAM hook handlers
-export([lookup_messages_hook/3]).

%% This value MUST be higher than the one in the backend being used (such as
%% mod_mam_riak_timed_arch_yz) so that the messages are processed
-define(LOOKUP_HOOK_PRIORITY, 100).


%%%===================================================================
%%% gen_mod implementation
%%%===================================================================

start(Host, _Opts) ->
    ejabberd_hooks:add(mam_lookup_messages, Host, ?MODULE,
                       lookup_messages_hook, ?LOOKUP_HOOK_PRIORITY),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(mam_lookup_messages, Host, ?MODULE,
                          lookup_messages_hook, ?LOOKUP_HOOK_PRIORITY),
    ok.

%%%===================================================================
%%% mam_lookup_messages callback
%%%===================================================================

-spec lookup_messages_hook(Result :: any(),
                           Host :: ejabberd:server(),
                           Params :: map()) ->
    {ok, mod_mam:lookup_result()} | {error, 'policy-violation'}.
lookup_messages_hook({error, _Reason} = Result, _Host, _Params) ->
    Result;
lookup_messages_hook({ok, {Count, Offset, MessageList}}, _Host,
                     #{rsm := #rsm_in{reverse = true}}) ->
    {ok, {Count, Offset, lists:reverse(MessageList)}};
lookup_messages_hook(Result, _Host, _Params) ->
    Result.
