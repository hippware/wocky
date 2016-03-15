-module(hxep_permissions).

-export([can_upload/2,
         can_download/2]).

-include_lib("ejabberd/include/jlib.hrl").

%% Avatars - only upload for yourself
can_upload(From, <<"avatar:", For/binary>>) ->
    jid:are_bare_equal(From, jid:from_binary(For));

%% Message media (inline images, videos etc) - upload any
can_upload(_, <<"message_media:", _/binary>>) -> true;

%% TODO: Multi-user chat? Other types?
can_upload(_, _) -> false.


can_download(User = #jid{lserver = Server}, FileID) ->
    case francus:open_read(Server, FileID) of
        {ok, File} ->
            Result = can_download_file(User, File),
            francus:close(File),
            Result;
        {error, not_found} ->
            {false, not_found}
    end.

can_download_file(User = #jid{luser = UserID}, File) ->
    case francus:owner(File) of
        UserID ->
            {true, UserID}; %% Users can always get their own files
        OwnerID ->
            can_download_with_metadata(User, OwnerID, francus:metadata(File))
    end.

can_download_with_metadata(User, OwnerID, #{<<"purpose">> := Purpose}) ->
    can_download_purpose(User, OwnerID, Purpose);
can_download_with_metadata(_, _, #{}) ->
    {false, no_purpose}.

%% Anyone can view avatars
can_download_purpose(_, OwnerID, <<"avatar:", _/binary>>) -> {true, OwnerID};

%% Media within messages is only viewable by the two parties involved in
%% the message
can_download_purpose(User, OwnerID, <<"message_media:", OtherID/binary>>) ->
    case jid:are_bare_equal(User, jid:from_binary(OtherID)) of
        true -> {true, OwnerID};
        false -> {false, permission_denied}
    end;

%% Default to false
can_download_purpose(_, _, _) ->
    {false, permission_denied}.
