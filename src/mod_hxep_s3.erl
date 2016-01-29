-module(mod_hxep_s3).

% Bucket: hxep-test
% Access Key ID: AKIAI4OZWBAA4SP6Y3WA
% Secret Access Key: 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

-include_lib("ejabberd/include/jlib.hrl").

-export([start/0,
         stop/0,
         make_download_response/3,
         make_upload_response/4]).

start() -> ok.

stop() -> ok.

make_download_response(FromJID, _ToJID, FileID) ->
    User = FromJID#jid.luser,

    Date = list_to_binary(httpd_util:rfc1123_date(erlang:localtime())),
    Headers = [
               {<<"host">>, host()},
               {<<"date">>, Date},
               {<<"authorization">>,
                make_auth(get, Date, "", User, FileID)}
              ],

    RespFields = [
                  {<<"url">>, s3_url(User, FileID)}
                 ],

    {Headers, RespFields}.

make_upload_response(FromJID, _ToJID, FileID, MimeType) ->
    User = FromJID#jid.luser,

    Date = list_to_binary(httpd_util:rfc1123_date(erlang:localtime())),
    Headers = [
               {<<"host">>, host()},
               {<<"content-type">>, MimeType},
               {<<"date">>, Date},
               {<<"authorization">>,
                make_auth(put, Date, MimeType, User, FileID)}
              ],

    RespFields = [
                  {<<"url">>, s3_url(User, FileID)},
                  {<<"method">>, <<"PUT">>}
                 ],

    {Headers, RespFields}.

s3_url(User, ID) ->
    iolist_to_binary(["https://", host(), "/", path(User, ID)]).

bucket() ->
    <<"hxep-test">>.

host() ->
    <<(bucket())/binary, ".s3.amazonaws.com">>.

path(User, File) ->
    [User, "/", File].

-spec make_auth(atom(), iodata(), iodata(), iodata(), iodata()) -> binary().
make_auth(Method, Date, MimeType, User, File) ->
    AccessKey = "AKIAI4OZWBAA4SP6Y3WA",
    SecretKey = "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",

    StringToSign = [string:to_upper(atom_to_list(Method)), $\n,
                    "", $\n, % ContentMD5
                    MimeType, $\n,
                    Date, $\n,
                    [], % Canonized Amazon headers
                    [$/, bucket(), $/],
                    path(User, File)
                   ],
    Signature = base64:encode(crypto:hmac(sha, SecretKey, StringToSign)),
    iolist_to_binary(["AWS ", AccessKey, $:, Signature]).

