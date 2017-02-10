%%% @copyright 2016+ Hippware, Inc.
%%% @doc Amazon AWS S3 backend for `mod_wocky_tros'
-module(mod_wocky_tros_s3).

-compile({parse_transform, cut}).
-compile({parse_transform, do}).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").
-include("wocky_db_seed.hrl").

-define(s3, 'Elixir.ExAws.S3').
-define(AWSConfig, 'Elixir.ExAws.Config').

-define(AMZ_CONTENT_TYPE, <<"content-type">>).

-behaviour(mod_wocky_tros_backend).

-export([start/1,
         stop/0,
         make_download_response/5,
         make_upload_response/6,
         get_owner/1,
         get_access/1,
         get_metadata/2,
         delete/2,
         keep/2,
         update_access/3
        ]).

%% Temporary exports while mod_wocky_tros_s3_legacy still exists:
-export([do_request/3, check_result_get_headers/2,
         get_metadata_item/2, bucket/0, path/2,
         access_key_id/0, secret_key/0]).

-define(LINK_EXPIRY, 60 * 10). % 10 minute expiry on upload/download links.

-ignore_xref([get_access/2]).

start(_Opts) ->
    Configs = [s3_bucket, s3_access_key_id, s3_secret_key],
    lists:foreach(fun(C) -> extract_config(C) end, Configs),
    ok.

stop() -> ok.

extract_config(Config) ->
    Value = wocky_app:get_config(Config),
    ejabberd_config:add_local_option(Config, Value).

make_download_response(_FromJID, #jid{lserver = LServer},
                       _OwnerID, FileID, _Metadata) ->
    RespFields = [
                  {<<"url">>, s3_url(LServer, bucket(), FileID, get, [])}
                 ],

    {[], RespFields}.

make_upload_response(FromJID = #jid{luser = Owner}, #jid{lserver = LServer},
                     FileID, _Size, Access,
                     #{?AMZ_CONTENT_TYPE := CT}) ->
    FileJID = jid:replace_resource(FromJID, <<"file/", FileID/binary>>),
    ReferenceURL = <<"tros:", (jid:to_binary(FileJID))/binary>>,

    RespFields = resp_fields(LServer, upload_bucket(), FileID,
                             put, [{?AMZ_CONTENT_TYPE, CT}], ReferenceURL),

    Headers = [{?AMZ_CONTENT_TYPE, CT}],

    ok = wocky_db:insert(shared, file_metadata,
                         #{id => FileID,
                           server => LServer,
                           access => Access,
                           owner => Owner}),
    {Headers, RespFields}.

resp_fields(LServer, Bucket, FileID, Method, URLParams, ReferenceURL) ->
    [
     {<<"method">>, list_to_binary(string:to_upper(atom_to_list(Method)))},
     {<<"url">>, s3_url(LServer, Bucket, FileID, Method, URLParams)},
     {<<"reference_url">>, ReferenceURL}
    ].

s3_url(Server, Bucket, FileID, Method, URLParams) ->
    Config = ?AWSConfig:new(s3, [{access_key_id, access_key_id()},
                                 {secret_access_key, secret_key()}]),
    Options = [
               {expires_in, ?LINK_EXPIRY},
               {virtual_host, false},
               {query_params, URLParams}
              ],
    {ok, URL} =
    ?s3:presigned_url(Config, Method, Bucket, path(Server, FileID), Options),
    URL.

update_access(Server, FileID, NewAccess) ->
    ok = wocky_db:insert(shared, file_metadata, #{id => FileID,
                                                  server => Server,
                                                  access => NewAccess}).

upload_bucket() ->
    <<(bucket())/binary, "-quarantine">>.

bucket() ->
    get_opt(s3_bucket).

access_key_id() ->
    get_opt(s3_access_key_id).

secret_key() ->
    get_opt(s3_secret_key).

get_opt(Opt) ->
    list_to_binary(ejabberd_config:get_local_option(Opt)).

path(Server, FileID) ->
    iolist_to_binary([Server, "-", hash_prefix(FileID), "/", FileID]).

hash_prefix(FileID) ->
    base16:encode(
      binary:part(crypto:hash(md5, FileID), 0, 2)).

get_owner(Metadata) ->
    get_metadata_item(Metadata, owner).

get_access(Metadata) ->
    get_metadata_item(Metadata, access).

get_metadata_item(Metadata, Item) ->
    ct:log("Metadata: ~p\nItem: ~p", [Metadata, Item]),
    case maps:get(Item, Metadata, undefined) of
        undefined ->
            {error, metadata_not_found};
        Value ->
            {ok, Value}
    end.

get_metadata(LServer, FileID) ->
    Row = wocky_db:select_row(shared, file_metadata, [owner, access],
                              #{id => FileID, server => LServer}),
    case Row of
        not_found ->
            {error, not_found};
        _ ->
            {ok, Row}
    end.

delete(LServer, FileID) ->
    do([error_m ||
        Result <- do_request(LServer, FileID, delete),
        check_result_get_headers(Result, 204),
        ok
       ]).

keep(_LServer, _FileID) ->
    %% S3 files don't currently expire, so keep() is a no-op.
    ok.

do_request(LServer, FileID, Type) ->
    R = httpc:request(Type,
                      {binary_to_list(s3_url(LServer, bucket(),
                                             FileID, Type, [])), []},
                      [], []),
    case R of
        {ok, _} -> R;
        {error, E} ->
            Text = list_to_binary(io_lib:fwrite("Error: ~p", [E])),
            {error, {retrieve_error, Text}}
    end.

check_result_get_headers({{_, Expected, _}, Headers, _Body}, Expected) ->
    {ok, Headers};
check_result_get_headers({{_, Code, _}, _Headers, _Body}, _Expected)
    when Code =:= 404 orelse
         Code =:= 403        %% S3 Likes to return 403 for non-existant files
         ->
    {error, not_found};
check_result_get_headers({Error, Headers, Body}, Expected) ->
    Text = iolist_to_binary(
             io_lib:fwrite("Error performing operation (expected ~p): ~p ~p ~p",
                           [Expected, Error, Headers, Body])),
    {error, {retrieve_error, Text}}.
