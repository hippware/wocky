%%% @copyright 2016+ Hippware, Inc.
%%% @doc Implementation module for HXEP file transfer system
%%% (https://github.com/hippware/tr-wiki/wiki/HXEP%3A-Files-over-http).
%%%
%%% TODO:
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
   make_file_id/0
        ]).

-export([
   validate_upload_size/1,
   validate_upload_type/1
        ]).

-ifdef(TEST).
-export([backend/0]).
-endif.

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").

-behaviour(gen_mod).

-define(DEFAULT_BACKEND, francus).
-define(DEFAULT_MAX_UPLOAD_SIZE, (1024*1024 * 10)). % 10MB

-record(request, {
          from_jid :: ejabberd:jid(),
          to_jid :: ejabberd:jid(),
          iq :: iq()
         }).

configs() ->
    %% Name in .cfg   |Name in ejabberd_config|Default value
    [{backend,         hxep_backend,          ?DEFAULT_BACKEND},
     {max_upload_size, hxep_max_upload_size, ?DEFAULT_MAX_UPLOAD_SIZE}
    ].

start(Host, Opts) ->
    lists:foreach(fun(C) -> set_config_from_opt(C, Opts) end, configs()),
    (backend()):start(Opts),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?HXEP_NS,
                                  ?MODULE, handle_iq, parallel).

stop(Host) ->
    _ = gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?HXEP_NS),
    (backend()):stop().

-spec handle_iq(From :: ejabberd:jid(),
                To :: ejabberd:jid(),
                IQ :: iq()) -> ignore | iq().
handle_iq(FromJID, ToJID, IQ = #iq{type = Type, sub_el = ReqEl}) ->
    Req = #request{from_jid = FromJID, to_jid = ToJID, iq = IQ},
    handle_request(Req, Type, ReqEl).

handle_request(Req, get, ReqEl = #xmlel{name = <<"download-request">>}) ->
    handle_download_request(Req, ReqEl);
handle_request(Req, set, ReqEl = #xmlel{name = <<"upload-request">>}) ->
    handle_upload_request(Req, ReqEl);
handle_request(_, _, _) ->
    ignore.

handle_download_request(Req = #request{iq = IQ}, DR) ->
    case extract_fields(DR, [<<"id">>], []) of
        {failed, Missing} ->
            send_missing_field_error(IQ, Missing);
        {ok, Fields} ->
            send_download_respone(Req, Fields)
    end.

handle_upload_request(Req = #request{iq = IQ}, UR) ->
    RequiredFields = [<<"filename">>, <<"size">>, <<"mime-type">>],
    OptionalFields = [<<"width">>, <<"height">>, <<"purpose">>],
    case extract_fields(UR, RequiredFields, OptionalFields) of
        {failed, Missing} ->
            send_missing_field_error(IQ, Missing);
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
        none -> {failed, H};
        _ -> check_fields(Fields, T)
    end.

send_missing_field_error(IQ, FieldName) ->
    Text = iolist_to_binary(["Required field missing: ", FieldName]),
    Error = ?ERRT_BAD_REQUEST(?MYLANG, Text),
    send_error_response(IQ, Error).

send_validation_error(IQ, ErrorStr) ->
    Text = iolist_to_binary(["Upload request denied: ", ErrorStr]),
    Error = ?ERRT_NOT_ACCEPTABLE(?MYLANG, Text),
    send_error_response(IQ, Error).

send_upload_response(Req = #request{iq = IQ}, ReqFields) ->
    MimeType = proplists:get_value(<<"mime-type">>, ReqFields),
    Name = proplists:get_value(<<"filename">>, ReqFields),
    Metadata = #{<<"content-type">> => MimeType,
                 <<"name">> => Name},
    Size = binary_to_integer_def(
             proplists:get_value(<<"size">>, ReqFields), 0),
    case validate_upload_req(Size, MimeType) of
        ok -> send_ok_upload_response(Req, Size, Metadata);
        {failed, Error} -> send_validation_error(IQ, Error)
    end.

validate_upload_req(Size, MimeType) ->
    Validations = [{validate_upload_size, Size, "Invalid file size"},
                   {validate_upload_type, MimeType, "File is an invalid type"}],
                   % Additional upload validations can go here

    Failures = lists:dropwhile(fun({F, P, _}) ->
                                       apply(?MODULE, F, [P]) end, Validations),
    case Failures of
        [] -> ok;
        [{_, _, Text}|_] -> {failed, Text}
    end.

validate_upload_size(Size) ->
    MaxSize = ejabberd_config:get_local_option(hxep_max_upload_size),
    Size =< MaxSize andalso Size > 0.

% TODO: configure a list of valid upload types
validate_upload_type(_MimeType) -> true.

send_ok_upload_response(Req = #request{from_jid = FromJID, to_jid = ToJID},
                        Size, Metadata) ->
    FileID = make_file_id(),
    {Headers, RespFields} =
        (backend()):make_upload_response(FromJID, ToJID, FileID,
                                         Size, Metadata),


    FullFields = common_fields(FromJID, FileID) ++ RespFields,
    send_response(Req, Headers, FullFields, <<"upload">>).

send_download_respone(Req = #request{from_jid = FromJID, to_jid = ToJID},
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

send_error_response(IQ = #iq{sub_el = SubEl}, Error) ->
    IQ#iq{type = error, sub_el = [SubEl, Error]}.

common_fields(#jid{luser = User, lserver = Server}, FileID) ->
    [{<<"id">>, FileID},
     {<<"jid">>, jid:to_binary(jid:make(User, Server, FileID))}].

to_header_element({Name, Value}) ->
    #xmlel{name = <<"header">>,
           attrs = [{<<"name">>, Name}, {<<"value">>, Value}]}.

to_xmlel({Name, Content}) ->
    #xmlel{name = Name, children = [#xmlcdata{content = Content}]}.

set_config_from_opt({CfgName, EJDName, Default}, Opts) ->
    Val = proplists:get_value(CfgName, Opts, Default),
    {atomic, _} = ejabberd_config:add_local_option(EJDName, Val),
    ok.

backend() ->
    list_to_atom("mod_hxep_" ++
      atom_to_list(
        ejabberd_config:get_local_option(hxep_backend))).

make_file_id() ->
    ossp_uuid:make(v1, text).

binary_to_integer_def(Binary, Default) ->
    try
        binary_to_integer(Binary)
    catch
        error:badarg -> Default
    end.
