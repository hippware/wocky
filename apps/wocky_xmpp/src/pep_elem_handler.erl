%%% @copyright 2016+ Hippware, Inc.
%%% @doc Behaviour definition for pep element handlers
%%% @see mod_wocky_pep.erl
-module(pep_elem_handler).
-include_lib("ejabberd/include/jlib.hrl").

%% @doc Callback that is called for each item passed through mod_wocky_pep.
%% The function should return either the item, an optionally modified version
%% of the item which will be used as the version forwarded to all subscribers,
%% or `drop' if the item should be dropped and not forwarded.
%%
%% `From': the Jid of the sender
%%
%% `Item': the sub-element of <item> being processed
%%
-callback handle_pep(From :: jid(), Item :: exml:element()) ->
    exml:element() | drop.
