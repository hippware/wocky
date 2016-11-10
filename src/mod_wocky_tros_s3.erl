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

-behaviour(mod_wocky_tros_backend).

-export([start/1,
         stop/0,
         make_download_response/5,
         make_upload_response/6,
         get_owner/1,
         get_access/1,
         get_metadata/2,
         delete/2,
         keep/2
        ]).

-define(ACCESS_KEY, <<65,127,89,223,29,138,212,158,163,0,171,96,6,190,
                      214,139,12,88,145,144,119,205,102,224,217,243,192,
                      85,164,247,57,127>>).
-define(PAD_TO, 128).
-define(AMZ_META_PREFIX, "x-amz-meta-").
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
                  {<<"url">>, s3_url(LServer, FileID, get, [])}
                 ],

    {[], RespFields}.

make_upload_response(#jid{luser = Owner}, #jid{lserver = LServer},
                     FileID, _Size, Access,
                     Metadata = #{<<"content-type">> := CT}) ->
    EncryptedAccess = wocky_crypto:encrypt(?ACCESS_KEY, Access, ?PAD_TO),
    AmzMetadata =
    Metadata#{<<?AMZ_META_PREFIX, "access">>
                 => base64:encode(EncryptedAccess),
              <<?AMZ_META_PREFIX, "owner">>
                 => Owner
             },
    URLParams = maps:to_list(AmzMetadata),
    RespFields = resp_fields(LServer, FileID, put, URLParams),

    Headers = [{<<"content-type">>, CT}],
    {Headers, RespFields}.

resp_fields(LServer, FileID, Method, URLParams) ->
    [
     {<<"method">>, list_to_binary(string:to_upper(atom_to_list(Method)))},
     {<<"url">>, s3_url(LServer, FileID, Method, URLParams)}
    ].

s3_url(Server, FileID, Method, URLParams) ->
    Config = ?AWSConfig:new(s3, [{access_key_id, access_key_id()},
                                 {secret_access_key, secret_key()}]),
    Options = [
               {expires_in, ?LINK_EXPIRY},
               {virtual_host, false},
               {query_params, URLParams}
              ],
    {ok, URL} =
    ?s3:presigned_url(Config, Method, bucket(), path(Server, FileID), Options),
    URL.

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
    get_metadata_item(Metadata, <<?AMZ_META_PREFIX, "owner">>).

get_access(Metadata) ->
    case get_metadata_item(Metadata, <<?AMZ_META_PREFIX, "access">>) of
        {ok, EncAccess} ->
            Access = wocky_crypto:decrypt(
                       ?ACCESS_KEY, base64:decode(EncAccess), true),
            {ok, Access};
        Error ->
            Error
    end.

get_metadata_item(Metadata, Item) ->
    case maps:get(Item, Metadata, undefined) of
        undefined ->
            {error, metadata_not_found};
        Value ->
            {ok, Value}
    end.

get_metadata(LServer, FileID) ->
    do([error_m ||
        Result <- do_request(LServer, FileID, head),
        Headers <- check_result_get_headers(Result, 200),
        get_metadata_items(Headers)
       ]).

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
                      {binary_to_list(s3_url(LServer, FileID, Type, [])), []},
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

get_metadata_items(Headers) ->
    List = [{list_to_binary(K), list_to_binary(http_uri:decode(V))}
            || {K, V} <- Headers],
    {ok, maps:from_list(List)}.
