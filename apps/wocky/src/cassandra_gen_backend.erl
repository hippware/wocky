%%%----------------------------------------------------------------------
%%% File    : cassandra_gen_backend.erl
%%% Author  : Beng Tan
%%% Purpose : Specification for cassandra backend modules
%%%
%%% Copyright (C) 2015 Hippware
%%%----------------------------------------------------------------------

-module(cassandra_gen_backend).

-callback aquery(Host :: binary(), 
                 Query :: binary() | string(),
                 Values :: [cassandra:value()], 
                 Consistency :: cassandra:consistency(),
                 PageSize :: non_neg_integer() | undefined) ->
            {ok, Result :: seestar_result:result()} | {error, Error :: seestar_error:error()}.

-callback pquery(Host :: binary(), 
                 Query :: binary() | string(),
                 Values :: [cassandra:value()], 
                 Consistency :: cassandra:consistency(),
                 PageSize :: non_neg_integer() | undefined) ->
            {ok, Result :: seestar_result:result()} | {error, Error :: seestar_error:error()}.

-callback pquery_async(Host :: binary(), 
                       Query :: binary() | string(),
                       Values :: [cassandra:value()], 
                       Consistency :: cassandra:consistency(),
                       PageSize :: non_neg_integer() | undefined) ->
            {ok, Result :: seestar_result:result()} | {error, Error :: seestar_error:error()}.

-callback rows(Rows :: cassandra:rows_result()) -> [[cassandra:value()]].
