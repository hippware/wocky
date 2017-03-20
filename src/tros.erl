%%% @copyright 2016+ Hippware, Inc.
%%% @doc TROS helper functions
-module(tros).

-export([parse_url/1,
         make_jid/2,
         make_url/2,

         get_metadata/2,
         get_access/1,
         get_owner/1,

         delete/2,

         get_base_id/1,
         get_type/1
        ]).

-type file_id() :: ejabberd:lresource().
-type url() :: <<_:40,_:_*8>>.
-type metadata() :: mod_wocky_tros_backend:metadata().
-type result(ResultType) :: {ok, ResultType} | {error, atom()}.
-type file_type() :: full | original | thumbnail.

-export_type([file_id/0, url/0, metadata/0, result/1]).

-include("tros.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").

-spec parse_url(url()) -> {ok, {ejabberd:lserver(), ejabberd:lresource()}} |
                          {error, invalid_url}.
parse_url(<<"tros:", JID/binary>>) ->
    #jid{lserver = Server, lresource = Resource} = jid:from_binary(JID),
    case Resource of
        <<"file/", FileID/binary>> -> {ok, {Server, FileID}};
        _ -> {error, invalid_url}
    end;
parse_url(_) ->
    {error, invalid_url}.

-spec make_jid(ejabberd:lserver(), ejabberd:resource()) -> ejabberd:jid().
make_jid(Server, FileID) ->
    jid:make(<<>>, Server, <<"file/", FileID/binary>>).

-spec make_url(ejabberd:lserver(), file_id()) -> url().
make_url(Server, FileID) ->
    <<"tros:", (jid:to_binary(make_jid(Server, FileID)))/binary>>.

-spec get_owner(metadata()) -> result(ejabberd:luser()).
get_owner(Metadata) ->
    get_file_info(get_owner, [Metadata]).

-spec get_access(metadata()) -> result(binary()).
get_access(Metadata) ->
    get_file_info(get_access, [Metadata]).

-spec get_metadata(ejabberd:lserver(), file_id()) -> result(metadata()).
get_metadata(Server, FileID) ->
    get_file_info(get_metadata, [Server, get_base_id(FileID)]).

get_file_info(Function, Args) ->
    apply(mod_wocky_tros:backend(), Function, Args).

-spec delete(ejabberd:lserver(), file_id()) -> ok.
delete(Server, FileID) ->
    apply(mod_wocky_tros:backend(), delete, [Server, FileID]).

-spec get_base_id(file_id()) -> file_id().
get_base_id(FileID) ->
    Split = binary:split(FileID, [?TROS_THUMBNAIL_SUFFIX,
                                  ?TROS_ORIGINAL_SUFFIX]),
    case Split of
        [BaseID, <<>>] -> BaseID;
        _ -> FileID
    end.

-spec get_type(file_id()) -> file_type().
get_type(FileID) ->
    OrigLen = byte_size(?TROS_ORIGINAL_SUFFIX),
    ThumbLen = byte_size(?TROS_THUMBNAIL_SUFFIX),
    OrigSuffix = binary:longest_common_suffix([FileID, ?TROS_ORIGINAL_SUFFIX]),
    case OrigSuffix of
        OrigLen ->
            original;
        _ ->
            ThumbSuffix = binary:longest_common_suffix(
                            [FileID, ?TROS_THUMBNAIL_SUFFIX]),
            case ThumbSuffix of
                ThumbLen -> thumbnail;
                _ -> full
            end
    end.
