%%%----------------------------------------------------------------------
%%% File    : wocky.hrl
%%% Author  : Beng Tan
%%% Purpose : A header stub
%%%
%%%
%%% Copyright (C) 2015 Hippware
%%%----------------------------------------------------------------------

-ifndef(WOCKY_HRL).
-define(WOCKY_HRL, true).

-define(WOCKY_VERSION, element(2, application:get_key(wocky,vsn))).

-record(table_def, {
          name        :: atom(),
          columns     :: [{atom(), atom()}],
          primary_key :: atom() | [atom()],
          order_by    :: atom() | [atom()]
         }).

-endif. % ifdef WOCKY_HRL
