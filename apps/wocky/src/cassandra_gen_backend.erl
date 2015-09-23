%%%----------------------------------------------------------------------
%%% File    : cassandra_gen_backend.erl
%%% Author  : Beng Tan
%%% Purpose : Specification for cassandra backend modules
%%%
%%% Copyright (C) 2015 Hippware
%%%----------------------------------------------------------------------

-module(cassandra_gen_backend).

% Temporarily use definitions from seestar. If we change backend, then this may change too.
-type value() :: seestar_cqltypes:value().
-type consistency() :: seestar:consistency().
-export_type([value/0, consistency/0]).

-callback aquery(Host :: ejabberd:lserver(), 
                 Query :: nonempty_string(),
                 Values :: [value()], 
                 Consistency :: consistency(),
                 PageSize :: non_neg_integer() | undefined) -> ok.

-callback pquery(Host :: ejabberd:lserver(), 
                 Query :: nonempty_string(),
                 Values :: [value()], 
                 Consistency :: consistency(),
                 PageSize :: non_neg_integer() | undefined) -> ok.

-callback pquery_async(Host :: ejabberd:lserver(), 
                       Query :: nonempty_string(),
                       Values :: [value()], 
                       Consistency :: consistency(),
                       PageSize :: non_neg_integer() | undefined) -> ok.
