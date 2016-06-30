-module(tros_permissions).

-export([can_upload/2,
         can_download/4]).

-include_lib("ejabberd/include/jlib.hrl").

%% Avatars - only upload for yourself
can_upload(_, <<"avatar">>) -> true;

%% Message media (inline images, videos etc) - upload any
can_upload(_, <<"message_media">>) -> true;

%% TODO: Multi-user chat? Other types?
can_upload(_, _) -> false.


can_download(User = #jid{luser = UserID}, OwnerID, Purpose, Access) ->
    case OwnerID of
        UserID ->
            true; %% Users can always get their own files
        _ ->
            can_download_purpose_access(User, Purpose, Access)
    end.

%% Anyone can view avatars
can_download_purpose_access(_, <<"avatar">>, _) -> true;

%% Media within messages is only viewable by the two parties involved in
%% the message
can_download_purpose_access(User, <<"message_media">>, Access) ->
    case matches_any_jid(User, jids_from_access_list(Access)) of
        true -> true;
        false -> {false, permission_denied}
    end;

%% Default to false
can_download_purpose_access(_, _, _) ->
    {false, permission_denied}.

jids_from_access_list(Access) ->
    JIDS = binary:split(Access, <<$,>>, [global, trim_all]),
    [jid:from_binary(J) || J <- JIDS].

matches_any_jid(User, JIDS) ->
    lists:any(fun(J) ->
                      jid:are_bare_equal(User, J)
              end,
              JIDS).
