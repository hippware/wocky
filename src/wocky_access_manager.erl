%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Behaviour module for modules providing access management
%%% See mod_wocky_access
%%%
-module(wocky_access_manager).

-export([check_access/4]).

-callback check_access(binary(), ejabberd:jid(), mod_wocky_access:op()) ->
    mod_wocky_access:access_result().

check_access(Node, Actor, Op, Module) ->
    Module:check_access(Node, jid:from_binary(Actor), Op).
