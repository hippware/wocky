-module(tros_permissions).

-export([can_upload/2,
         can_download/3]).

-include_lib("ejabberd/include/jlib.hrl").

%% Avatars - only upload for yourself
can_upload(From, <<"avatar:", For/binary>>) ->
    jid:are_bare_equal(From, jid:from_binary(For));

%% Message media (inline images, videos etc) - upload any
can_upload(_, <<"message_media:", _/binary>>) ->
    true;

%% Group chat media can be uploaded by any member of the specified chat
can_upload(From, <<"group_chat_media:", ChatID/binary>>) ->
    mod_wocky_group_chat:is_participant(From, ChatID);

%% TODO: Other types?
can_upload(_, _) -> false.


can_download(User = #jid{luser = UserID}, OwnerID, Metadata) ->
    case OwnerID of
        UserID ->
            true; %% Users can always get their own files
        _ ->
            can_download_with_metadata(User, Metadata)
    end.

can_download_with_metadata(User, #{<<"purpose">> := Purpose}) ->
    can_download_purpose(User, Purpose);
can_download_with_metadata(_, #{}) ->
    {false, no_purpose}.

%% Anyone can view avatars
can_download_purpose(_, <<"avatar:", _/binary>>) -> true;

%% Media within messages is only viewable by the two parties involved in
%% the message
can_download_purpose(User, <<"message_media:", OtherID/binary>>) ->
    case jid:are_bare_equal(User, jid:from_binary(OtherID)) of
        true -> true;
        false -> {false, permission_denied}
    end;

%% Group chat media can be downloaded by any member of the chat.
can_download_purpose(User, <<"group_chat_media:", ChatID/binary>>) ->
    case mod_wocky_group_chat:is_participant(User, ChatID) of
        true -> true;
        false -> {false, permission_denied}
    end;

%% Default to false
can_download_purpose(_, _) ->
    {false, permission_denied}.
