%%% @copyright 2016+ Hippware, Inc.
%%% @doc Behaviour definition for mod_wocky_tros backends
-module(mod_wocky_tros_backend).

-type error() :: not_found | metadata_not_found | {retrieve_error, binary()}.
-type result(ResultType) :: {ok, ResultType} | {error, error()}.
-type metadata() :: #{binary() => binary()}.

-callback start(list()) -> any().

-callback stop() -> any().

-callback get_owner(tros:file_id()) -> result(ejabberd:luser()).

-callback get_access(tros:file_id()) -> result(binary()).

-callback delete(ejabberd:lserver(), tros:file_id()) -> ok.

-callback make_upload_response(ejabberd:jid(),
                               ejabberd:jid(),
                               binary(),
                               integer(),
                               binary(),
                               metadata()) ->
    {list(), list()}.

-callback make_download_response(ejabberd:jid(),
                                 ejabberd:jid(),
                                 binary(),
                                 binary(),
                                 metadata()) ->
    {list(), list()}.
