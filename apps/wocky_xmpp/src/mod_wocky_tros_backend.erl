%%% @copyright 2016+ Hippware, Inc.
%%% @doc Behaviour definition for mod_wocky_tros backends
-module(mod_wocky_tros_backend).

-type metadata() :: #{binary() => binary()}.
-type error() :: not_found | metadata_not_found | {retrieve_error, binary()}.
-type result(ResultType) :: {ok, ResultType} | {error, error()}.

-export_type([metadata/0]).

-callback start(list()) -> any().

-callback stop() -> any().

-callback get_owner(metadata()) -> result(ejabberd:luser()).

-callback get_access(metadata()) -> result(binary()).

-callback get_metadata(ejabberd:lserver(), tros:file_id()) ->
    result(metadata()).

-callback delete(ejabberd:lserver(), tros:file_id()) -> ok.

-callback make_upload_response(ejabberd:jid(),
                               ejabberd:jid(),
                               binary(),
                               integer(),
                               binary(),
                               map()) ->
    {list(), list()}.

-callback make_download_response(ejabberd:jid(),
                                 ejabberd:jid(),
                                 binary(),
                                 binary(),
                                 map()) ->
    {list(), list()}.
