%%% @copyright 2016+ Hippware, Inc.
%%% @doc Amazon AWS S3 backend for `mod_tros'
-module(mod_tros_s3).

-include_lib("ejabberd/include/jlib.hrl").

-export([start/1,
         stop/0,
         make_download_response/4,
         make_upload_response/5,
         make_auth/5
        ]).

start(Opts) ->
    Configs = [s3_bucket, s3_access_key_id, s3_secret_key],
    lists:foreach(fun(C) -> extract_config(Opts, C) end, Configs),
    ok.

stop() -> ok.

extract_config(Opts, Config) ->
    Value = proplists:get_value(Config, Opts),
    ejabberd_config:add_local_option(Config, Value).

make_download_response(_FromJID, _ToJID, OwnerID, FileID) ->
    Date = list_to_binary(httpd_util:rfc1123_date(erlang:localtime())),
    Headers = [
               {<<"host">>, host()},
               {<<"date">>, Date},
               {<<"authorization">>,
                %% Explicit module named added to allow us to mock
                %% out make_auth/0 for testing
                ?MODULE:make_auth(get, Date, "", OwnerID, FileID)}
              ],

    RespFields = [
                  {<<"url">>, s3_url(OwnerID, FileID)}
                 ],

    {Headers, RespFields}.

make_upload_response(FromJID, _ToJID, FileID, _Size,
                     #{<<"content-type">> := ContentType}) ->
    User = FromJID#jid.luser,

    Date = list_to_binary(httpd_util:rfc1123_date(erlang:localtime())),
    Headers = [
               {<<"host">>, host()},
               {<<"content-type">>, ContentType},
               {<<"date">>, Date},
               {<<"authorization">>,
                %% Explicit module name added to allow us to mock
                %% out make_auth/5 for testing
                ?MODULE:make_auth(put, Date, ContentType, User, FileID)}
              ],

    RespFields = [
                  {<<"url">>, s3_url(User, FileID)},
                  {<<"method">>, <<"PUT">>}
                 ],

    {Headers, RespFields}.

s3_url(User, ID) ->
    iolist_to_binary(["https://", host(), "/", path(User, ID)]).

bucket() ->
    ejabberd_config:get_local_option(s3_bucket).

host() ->
    <<(list_to_binary(bucket()))/binary, ".s3.amazonaws.com">>.

access_key_id() ->
    ejabberd_config:get_local_option(s3_access_key_id).

secret_key() ->
    ejabberd_config:get_local_option(s3_secret_key).

path(User, File) ->
    [User, "/", File].

-spec make_auth(atom(), iodata(), iodata(), iodata(), iodata()) -> binary().
make_auth(Method, Date, MimeType, User, File) ->
    StringToSign = [string:to_upper(atom_to_list(Method)), $\n,
                    "", $\n, % ContentMD5
                    MimeType, $\n,
                    Date, $\n,
                    [], % Canonized Amazon headers
                    [$/, bucket(), $/],
                    path(User, File)
                   ],
    Signature = base64:encode(crypto:hmac(sha, secret_key(), StringToSign)),
    iolist_to_binary(["AWS ", access_key_id(), $:, Signature]).

