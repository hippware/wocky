%%% @copyright 2015+ Hippware, Inc.
%%% @doc Behaviour specification for cassandra backend modules

-module(wocky_db_backend).


-callback configure(Host :: string(), Config :: [proplists:property()]) -> ok | {error, any()}.

-callback clear() -> ok.

-callback aquery(Host :: binary(),
                 Query :: binary() | string(),
                 Values :: [wocky_db:value()],
                 Consistency :: wocky_db:consistency(),
                 PageSize :: non_neg_integer() | undefined) ->
            {ok, Result :: seestar_result:result()} | {error, Error :: seestar_error:error()}.

-callback pquery(Host :: binary(),
                 Query :: binary() | string(),
                 Values :: [wocky_db:value()],
                 Consistency :: wocky_db:consistency(),
                 PageSize :: non_neg_integer() | undefined) ->
            {ok, Result :: seestar_result:result()} | {error, Error :: seestar_error:error()}.

-callback pquery_async(Host :: binary(),
                       Query :: binary() | string(),
                       Values :: [wocky_db:value()],
                       Consistency :: wocky_db:consistency(),
                       PageSize :: non_neg_integer() | undefined) ->
            {ok, Result :: seestar_result:result()} | {error, Error :: seestar_error:error()}.

-callback batch_pquery(Host :: binary(),
                 Queries :: [{binary() | string(), [cassandra:value()]}],
                 Type :: logged | unlogged | counter,
                 Consistency :: wocky_db:consistency()) ->
            {ok, Result :: seestar_result:result()} | {error, Error :: seestar_error:error()}.

-callback rows(Rows :: wocky_db:rows_result()) -> [[wocky_db:value()]].
