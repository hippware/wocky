%%% @copyright 2016+ Hippware, Inc.
%%% @doc Implementation module for HXEP file transfer system
%%% (https://github.com/hippware/tr-wiki/wiki/HXEP%3A-Files-over-http).
%%%
%%% TODO:
%%% * Implement check that upload size matches stated file size
%%% * Implement multi-read/multi-response sends
%%% * Implement permission system
%%% * Impelment purpose system?
%%% * Store supplied filename?
%%%
-module(mod_hxep).

-define(HXEP_NS, <<"hippware.com/hxep/http-file">>).

-export([
   start/2,
   stop/1,
   handle_iq/3,

   % Exports for testing only:
   backend/0
        ]).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").

-behaviour(gen_mod).

-define(DEFAULT_BACKEND, francus).

-record(request, {
          from_jid :: ejabberd:jid(),
          to_jid :: ejabberd:jid(),
          iq :: iq()
         }).

start(Host, Opts) ->
    set_backend_from_opts(Opts),
    (backend()):start(),
    ejabberd_sm:register_iq_handler(Host, ?HXEP_NS, ?MODULE, handle_iq).

stop(Host) ->
    _ = ejabberd_sm:unregister_iq_handler(Host, ?HXEP_NS),
    (backend()):stop().

-spec handle_iq(From :: ejabberd:jid(),
                To :: ejabberd:jid(),
                IQ :: iq()) -> ignore | iq().
handle_iq(FromJID, ToJID, IQ = #iq{type = Type, sub_el = ReqEl}) ->
    Req = #request{from_jid = FromJID, to_jid = ToJID, iq = IQ},
    case Type of
        get -> do_get(Req, ReqEl);
        set -> do_set(Req, ReqEl);
        _ -> ignore
    end.

do_get(Req, ReqEl = #xmlel{name = <<"download-request">>}) ->
    process_download_request(Req, ReqEl);
do_get(_, _) -> ignore.

do_set(Req, ReqEl = #xmlel{name = <<"upload-request">>}) ->
    process_upload_request(Req, ReqEl);
do_set(_, _) -> ignore.

process_download_request(Req, DR) ->
    case extract_fields(DR, [<<"id">>], []) of
        failed ->
            ignore;
        {ok, Fields} ->
            send_download_resposne(Req, Fields)
    end.

process_upload_request(Req, UR) ->
    RequiredFields = [<<"filename">>, <<"size">>, <<"mime-type">>],
    OptionalFields = [<<"width">>, <<"height">>, <<"purpose">>],
    case extract_fields(UR, RequiredFields, OptionalFields) of
        failed ->
            ignore;
        {ok, Fields} ->
            send_upload_response(Req, Fields)
    end.

extract_fields(Req, RequiredFields, OptionalFields) ->
    Fields = lists:foldl(fun(F, Acc) -> add_field(Req, F, Acc) end, [],
                         RequiredFields ++ OptionalFields),
    check_fields(Fields, RequiredFields).

add_field(UR, Field, Acc) ->
    case exml_query:path(UR, [{element, Field}, cdata]) of
        undefined ->
            Acc;
        Value when is_binary(Value) ->
            [{Field, Value} | Acc]
    end.

check_fields(Fields, []) -> {ok, Fields};
check_fields(Fields, [H|T]) ->
    case proplists:lookup(H, Fields) of
        none -> failed;
        _ -> check_fields(Fields, T)
    end.

send_upload_response(Req = #request{from_jid = FromJID, to_jid = ToJID},
                     ReqFields) ->
    FileID = ossp_uuid:make(v1, text),
    MimeType = proplists:get_value(<<"mime-type">>, ReqFields),

    {Headers, RespFields} =
        (backend()):make_upload_response(FromJID, ToJID, FileID, MimeType),

    FullFields = common_fields(FromJID, ToJID, FileID) ++ RespFields,
    send_response(Req, Headers, FullFields, <<"upload">>).

send_download_resposne(Req = #request{from_jid = FromJID, to_jid = ToJID},
                       ReqFields) ->
    FileID = proplists:get_value(<<"id">>, ReqFields),

    {Headers, RespFields} = (backend()):make_download_response(FromJID,
                                                            ToJID, FileID),

    send_response(Req, Headers, RespFields, <<"download">>).

send_response(#request{iq = IQ}, Headers, RespFields, RespType) ->
    HeaderElement = #xmlel{name = <<"headers">>,
                           children =
                           [to_header_element(H) || H <- Headers]},

    ActionElement = #xmlel{name = RespType,
                           children =
                           [HeaderElement |
                            [to_xmlel(F) || F <- RespFields]]},

    IQ#iq{type = result, sub_el = ActionElement}.

common_fields(FromJID, ToJID, FileID) ->
    User = FromJID#jid.luser,
    Server = ToJID#jid.lserver,
    [{<<"id">>, FileID},
     {<<"jid">>, jid:to_binary(jid:make(User, Server, FileID))}].

to_header_element({Name, Value}) ->
    #xmlel{name = <<"header">>,
           attrs = [{<<"name">>, Name}, {<<"value">>, Value}]}.

to_xmlel({Name, Content}) ->
    #xmlel{name = Name, children = [#xmlcdata{content = Content}]}.

set_backend_from_opts(Otps) ->
    Backend = proplists:get_value(backend, Otps, ?DEFAULT_BACKEND),
    {atomic, _} = ejabberd_config:add_local_option(hxep_backend, Backend),
    ok.

backend() ->
    list_to_atom("mod_hxep_" ++
      atom_to_list(
        ejabberd_config:get_local_option(hxep_backend))).
