-module(tros_permissions).

-export([can_upload/3,
         can_download/4]).

-include_lib("ejabberd/include/jlib.hrl").

%% Avatars - only upload for yourself
can_upload(_, <<"avatar">>, _) ->
    true;

%% Message media (inline images, videos etc) - upload any
can_upload(_, <<"message_media">>, _) ->
    true;

%% Group chat media can be uploaded by any member of (all) the specified chat(s)
can_upload(From, <<"group_chat_media">>, ChatID) ->
    Chats = jids_from_access_list(ChatID),
    lists:all(fun(X) -> X end,
              [mod_wocky_group_chat:is_participant(From, C) || C <- Chats]);

%% TODO: Other types?
can_upload(_, _, _) ->
    false.


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

%% Group chat media can be downloaded by any member of (any of) the chat(s).
can_download_purpose_access(User, <<"group_chat_media">>, Access) ->
    Chats = jids_from_access_list(Access),
    Membership = [mod_wocky_group_chat:is_participant(User, C) || C <- Chats],
    case lists:any(fun(X) -> X end, Membership) of
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
