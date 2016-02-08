%%% @copyright 2016+ Hippware, Inc.
%%% @doc Test suite for mod_hxep.erl
-module(mod_hxep_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/jlib.hrl").

-define(SERVER, "localhost").

mod_hxep_test_() -> {
  "mod_hxep",
  [{setup, fun() -> before_all(Backend) end, fun after_all/1,
    [
     test_upload_request(Backend),
     test_download_request(Backend)
    ]}
   || Backend <- [s3, francus]
  ]
}.

mecks() -> [ejabberd_config, ejabberd_sm, httpd_util, ossp_uuid,
            cowboy, mod_hxep_francus].

before_all(Backend) ->
    ok = wocky_app:start(),
    lists:foreach(fun(M) -> meck:new(M, [passthrough]) end, mecks()),
    meck:expect(ejabberd_config, add_local_option, 2, {atomic, ok}),
    meck:expect(ejabberd_config, get_local_option,
                fun(hxep_backend) -> Backend;
                   (s3_bucket) -> "hxep-test";
                   (s3_access_key_id) -> "AKIAI4OZWBAA4SP6Y3WA";
                   (s3_secret_key) -> "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"
                end),
    meck:expect(ejabberd_config, get_global_option,
                fun(francus_chunk_size) -> undefined % Use the default
                end),

    meck:expect(gen_iq_handler, add_iq_handler, 6, ok),
    meck:expect(gen_iq_handler, remove_iq_handler, 3, ok),

    meck:expect(httpd_util, rfc1123_date, 1,
                "Fri, 29 Jan 2016 02:54:44 GMT"),

    meck:expect(ossp_uuid, make, 2, file_uuid()),

    meck:expect(cowboy, start_https, 4, ok),

    meck:expect(mod_hxep_francus, make_auth,
                fun() -> base64:encode(binary:copy(<<6:8>>, 128)) end),

    mod_hxep:start(?SERVER, []).

after_all(_) ->
    mod_hxep:stop(?SERVER),
    ok = wocky_app:stop(),
    lists:foreach(fun(M) -> true = meck:validate(M) end, mecks()),
    lists:foreach(fun meck:unload/1, mecks()).

before_each() ->
    meck:new(ejabberd_router),
    ok.

after_each(_) ->
    meck:unload(ejabberd_router),
    ok.

test_upload_request(Backend) ->
    {"Upload request", [
        {"Successful request", foreach, fun before_each/0, fun after_each/1, [
          ?_assertEqual(
             expected_upload_packet(Backend),
             exml:to_binary(
               jlib:iq_to_xml(
                 mod_hxep:handle_iq(test_user_jid(),
                                    test_server_jid(),
                                    upload_packet()))))
    ]}]}.

test_download_request(Backend) ->
    {"Download request", [
        {"Successful request", foreach, fun before_each/0, fun after_each/1,
         [
          ?_assertEqual(
             expected_download_packet(Backend),
             exml:to_binary(
               jlib:iq_to_xml(
                 mod_hxep:handle_iq(test_user_jid(),
                                    test_server_jid(),
                                    download_packet()))))
    ]}]}.

file_uuid() ->
    <<"a65ecb4e-c633-11e5-9fdc-080027f70e96">>.

test_user() ->
    <<"testuser@localhost.example.com/myphone">>.

test_user_jid() -> jid:from_binary(test_user()).

test_server() ->
    <<"localhost.example.com">>.

test_server_jid() -> jid:from_binary(test_server()).

common_packet(Type, Request) ->
    #iq{id = <<"123456">>,
        type = Type,
        sub_el = Request
       }.

upload_packet() ->
    common_packet(set, upload_request()).

upload_request() ->
    Elements = [{<<"filename">>, <<"photo.jpeg">>},
                {<<"size">>, <<"10000">>},
                {<<"mime-type">>, <<"image/jpeg">>}],
    #xmlel{name = <<"upload-request">>,
           attrs = [{<<"xmlns">>, <<"hippware.com/hxep/http-file">>}],
           children = [
                       #xmlel{name = N,
                              children = [#xmlcdata{content = C}]}
                       || {N, C} <- Elements]}.

download_packet() ->
    common_packet(get, download_request()).

download_request() ->
    #xmlel{name = <<"download-request">>,
           attrs = [{<<"xmlns">>, <<"hippware.com/hxep/http-file">>}],
           children = [#xmlel{name = <<"id">>,
                              children = [#xmlcdata{content = file_uuid()}]}]}.

expected_upload_packet(s3) ->
    <<"<iq id='123456' type='result'><upload><headers>"
      "<header name='host' value='hxep-test.s3.amazonaws.com'/>"
      "<header name='content-type' value='image/jpeg'/>"
      "<header name='date' value='Fri, 29 Jan 2016 02:54:44 GMT'/>"
      "<header name='authorization' value='AWS "
      "AKIAI4OZWBAA4SP6Y3WA:a8BaqblNIH3GUD+WVS9II8g3lrc='/></headers>"
      "<id>a65ecb4e-c633-11e5-9fdc-080027f70e96</id>"
      "<jid>testuser@localhost.example.com/a65ecb4e-c633-11e5-9fdc-080027f70e96"
      "</jid><url>https://hxep-test.s3.amazonaws.com/testuser/"
      "a65ecb4e-c633-11e5-9fdc-080027f70e96</url><method>PUT</method>"
      "</upload></iq>">>;

expected_upload_packet(francus) ->
    <<"<iq id='123456' type='result'><upload><headers>"
      "<header name='content-type' value='image/jpeg'/>"
      "<header name='authorization' value='BgYGBgYGBgYGBgYGBgYGBgYG"
      "BgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYG"
      "BgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYG"
      "BgYGBgYGBgYGBgYGBgYGBgYGBgY='/></headers>"
      "<id>a65ecb4e-c633-11e5-9fdc-080027f70e96</id>"
      "<jid>testuser@localhost.example.com/"
      "a65ecb4e-c633-11e5-9fdc-080027f70e96</jid>"
      "<url>https://localhost.example.com:1025/users/testuser/"
      "files/a65ecb4e-c633-11e5-9fdc-080027f70e96</url>"
      "<method>PUT</method></upload></iq>">>.

expected_download_packet(s3) ->
    <<"<iq id='123456' type='result'><download><headers>"
      "<header name='host' value='hxep-test.s3.amazonaws.com'/>"
      "<header name='date' value='Fri, 29 Jan 2016 02:54:44 GMT'/>"
      "<header name='authorization' value='AWS "
      "AKIAI4OZWBAA4SP6Y3WA:5zE+m/BF+bc1wMa68p2rHeMfkCQ='/></headers>"
      "<url>https://hxep-test.s3.amazonaws.com/testuser/"
      "a65ecb4e-c633-11e5-9fdc-080027f70e96</url></download></iq>">>;

expected_download_packet(francus) ->
    <<"<iq id='123456' type='result'><download><headers>"
      "<header name='authorization' value='BgYGBgYGBgYGBgYGBgYGBgYGBgYG"
      "BgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYG"
      "BgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYG"
      "BgYGBgYGBgYGBgY='/></headers>"
      "<url>https://localhost.example.com:1025/users/testuser/"
      "files/a65ecb4e-c633-11e5-9fdc-080027f70e96</url>"
      "<method>GET</method></download></iq>">>.
