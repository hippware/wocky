%%% @copyright 2016+ Hippware, Inc.
%%% @doc Amazon AWS S3 backend for `mod_wocky_tros'
-module(mod_wocky_tros_s3).

-compile({parse_transform, cut}).
-compile({parse_transform, do}).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").
-include("wocky.hrl").
-include("wocky_db_seed.hrl").
-include("tros.hrl").

-define(s3, 'Elixir.ExAws.S3').
-define(auth, 'Elixir.ExAws.Auth').
-define(AWSConfig, 'Elixir.ExAws.Config').

-define(AMZ_CONTENT_TYPE, <<"content-type">>).

-behaviour(mod_wocky_tros_backend).

-export([start/1,
         stop/0,
         make_download_response/5,
         make_upload_response/6,
         get_owner/1,
         get_access/1,
         delete/2,
         update_access/2
        ]).

-ifdef(TEST).
-export([path/2]).
-endif.

-define(LINK_EXPIRY, 60 * 10). % 10 minute expiry on upload/download links.

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
                     FileID, Size, Access,
                     #{?AMZ_CONTENT_TYPE := CT}) ->
    FileJID = jid:replace_resource(FromJID, <<"file/", FileID/binary>>),
    ReferenceURL = <<"tros:", (jid:to_binary(FileJID))/binary>>,

    Headers =
    [
     {<<"x-amz-content-sha256">>, <<"UNSIGNED-PAYLOAD">>},
     {<<"content-length">>, integer_to_binary(Size)},
     {?AMZ_CONTENT_TYPE, CT}
    ],

    Config = ?AWSConfig:new(s3, [{access_key_id, access_key_id()},
                                 {secret_access_key, secret_key()}]),

    URL = <<"https://", (upload_bucket())/binary, ".s3.amazonaws.com/",
            (path(LServer, FileID))/binary>>,

    {ok, RetHeaders} = ?auth:headers(put, URL, s3, Config, Headers, nil),

    RespFields = resp_fields(put, URL, ReferenceURL),

    ?wocky_tros_metadata:put(FileID, Owner, Access),

    {RetHeaders, RespFields}.

resp_fields(Method, URL, ReferenceURL) ->
    [
     {<<"method">>, list_to_binary(string:to_upper(atom_to_list(Method)))},
     {<<"url">>, URL},
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

update_access(FileID, NewAccess) ->
    ?wocky_tros_metadata:set_access(FileID, NewAccess).

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
      binary:part(crypto:hash(md5, tros:get_base_id(FileID)), 0, 2)).

get_owner(ID) ->
    case ?wocky_tros_metadata:get_user_id(ID) of
        nil -> {error, not_found};
        Owner -> {ok, Owner}
    end.

get_access(ID) ->
    case ?wocky_tros_metadata:get_access(ID) of
        nil -> {error, not_found};
        Owner -> {ok, Owner}
    end.

delete(LServer, FileID) ->
    Files = [<<FileID/binary, Suffix/binary>> ||
             Suffix <- [<<>>, ?TROS_THUMBNAIL_SUFFIX, ?TROS_ORIGINAL_SUFFIX]],
    lists:foreach(do_delete(LServer, _), Files).

do_delete(LServer, FileID) ->
    do([error_m ||
        Result <- do_request(LServer, FileID, delete),
        check_result_get_headers(Result, 204),
        ok
       ]).

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
