-module(mod_wocky_tros_s3_legacy).

-compile({parse_transform, do}).

-include_lib("ejabberd/include/jlib.hrl").

-export([get_metadata/2,
         get_owner/1,
         get_access/1,
         get_content_type/1,
         update_access/3,
         make_upload_response/6
        ]).

-import(mod_wocky_tros_s3, [do_request/3, check_result_get_headers/2,
                            get_metadata_item/2, bucket/0, path/2,
                            access_key_id/0, secret_key/0]).

-define(s3, 'Elixir.ExAws.S3').
-define(AWSConfig, 'Elixir.ExAws.Config').
-define(ex_aws, 'Elixir.ExAws').

-define(LINK_EXPIRY, 60 * 10). % 10 minute expiry on upload/download links.
-define(AMZ_META_PREFIX, "x-amz-meta-").
-define(ACCESS_KEY, <<65,127,89,223,29,138,212,158,163,0,171,96,6,190,
                      214,139,12,88,145,144,119,205,102,224,217,243,192,
                      85,164,247,57,127>>).
-define(AMZ_CONTENT_TYPE, <<"content-type">>).
-define(PAD_TO, 128).

get_metadata(LServer, FileID) ->
    do([error_m ||
        Result <- do_request(LServer, FileID, head),
        Headers <- check_result_get_headers(Result, 200),
        get_metadata_items(Headers)
       ]).

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

get_content_type(Metadata) ->
    get_metadata_item(Metadata, ?AMZ_CONTENT_TYPE).

get_metadata_items(Headers) ->
    List = [{list_to_binary(K), list_to_binary(http_uri:decode(V))}
            || {K, V} <- Headers],
    {ok, maps:from_list(List)}.

encrypt_access(Access) ->
    base64:encode(wocky_crypto:encrypt(?ACCESS_KEY, Access, ?PAD_TO)).

update_access(Server, FileID, NewAccess) ->
    do([error_m ||
        Metadata    <- get_metadata(Server, FileID),
        Owner       <- get_owner(Metadata),
        ContentType <- get_content_type(Metadata),
        NewMetadata <- new_metadata(NewAccess, Owner),
        Request <- {ok, ?s3:put_object_copy(
                           bucket(),
                           path(Server, FileID),
                           bucket(),
                           path(Server, FileID),
                           [{metadata_directive, 'REPLACE'},
                            {content_type, ContentType},
                            {meta, NewMetadata}])},
        ?ex_aws:request(Request,
                        [{access_key_id, access_key_id()},
                         {secret_access_key, secret_key()}]),
        ok
       ]).

new_metadata(NewAccess, Owner) ->
    {ok,
     [{<<"access">>, encrypt_access(NewAccess)},
      {<<"owner">>, Owner}]}.

make_upload_response(FromJID = #jid{luser = Owner}, #jid{lserver = LServer},
                     FileID, _Size, Access,
                     Metadata = #{?AMZ_CONTENT_TYPE := CT}) ->
    FileJID = jid:replace_resource(FromJID, <<"file/", FileID/binary>>),
    ReferenceURL = <<"tros:", (jid:to_binary(FileJID))/binary>>,
    EncryptedAccess = encrypt_access(Access),
    AmzMetadata =
    Metadata#{<<?AMZ_META_PREFIX, "access">>
              => EncryptedAccess,
              <<?AMZ_META_PREFIX, "owner">>
              => Owner
             },
    URLParams = maps:to_list(AmzMetadata),
    RespFields = resp_fields(LServer, FileID, put, URLParams, ReferenceURL),

    Headers = [{?AMZ_CONTENT_TYPE, CT}],
    {Headers, RespFields}.

resp_fields(LServer, FileID, Method, URLParams, ReferenceURL) ->
    [
     {<<"method">>, list_to_binary(string:to_upper(atom_to_list(Method)))},
     {<<"url">>, s3_url(LServer, FileID, Method, URLParams)},
     {<<"reference_url">>, ReferenceURL}
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
