%%% @copyright 2016+ Hippware, Inc.
%%% @doc Behaviour definition for pep element handlers
%%% @see mod_wocky_pep.erl
-module(pep_elem_handler).
-include_lib("ejabberd/include/jlib.hrl").

-ignore_xref([{behaviour_info, 1}]).

-callback handle_pep(From :: jid(), Item :: exml:element()) ->
    exml:element() | undefined.
