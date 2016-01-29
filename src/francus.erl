-module(francus).

-export([
         open_write/4,
         open_read/2,
         read/1,
         read/2,
         write/2,
         close/1,
         delete/2,
         owner/1,
         content_type/1,
         size/1,

         % Exported for testing purposes only
         default_chunk_size/0
        ]).

-export_type([
              francus_file/0
             ]).

-record(state, {
          file_id :: binary(),
          user_id :: binary(),
          content_type :: binary(),
          context :: wocky_db:context(),
          pending = <<>> :: binary(),
          size = 0 :: non_neg_integer(),
          committed_size = 0 :: non_neg_integer(),
          chunks = [] :: [binary()]
         }).

-opaque francus_file() :: #state{}.

-define(DEFAULT_CHUNK_SIZE, 1024 * 1024).

%%% WRITE %%%

-spec open_write(wocky_db:context(), binary(), binary(), binary()) ->
    {ok, francus_file()}.
open_write(Context, FileID, UserID, ContentType) ->
    Values = #{id => FileID,
               user => UserID,
               size => 0,
               content_type => ContentType},
    ok = wocky_db:insert(Context, media, Values),
    {ok, #state{file_id = FileID,
                user_id = UserID,
                content_type = ContentType,
                context = Context}}.

-spec write(francus_file(), binary()) -> francus_file().
write(S = #state{pending = Pending, size = Size}, Data) ->
    NewData = <<Pending/binary, Data/binary>>,
    NewSize = Size + byte_size(Data),
    maybe_commit(S#state{size = NewSize}, NewData).

maybe_commit(State, Data) ->
    ChunkSize = chunk_size(),
    case byte_size(Data) of
        X when X < ChunkSize -> State#state{pending = Data};
        _ -> commit(State, Data)
    end.

commit(S = #state{file_id = FileID, committed_size = CS, context = Context},
       Data) ->
    ChunkSize = chunk_size(),
    <<ToWrite:ChunkSize/binary, Remaining/binary>> = Data,
    NewCommittedSize = CS + byte_size(ToWrite),
    commit_chunk(FileID, Context, NewCommittedSize, ToWrite),
    maybe_commit(S#state{committed_size = NewCommittedSize}, Remaining).

commit_chunk(FileID, Context, NewCommittedSize, Data) ->
    % This would ideally be a batch query. However apparently C* doesn't
    % particularly like batches where the total data size is > 50kb. This is
    % configurable (batch_size_fail_threshold_in_kb) but the comments
    % specifically advise against it, waving their hands about "node
    % instability". Which sounds bad.
    ChunkID = ossp_uuid:make(v1, text),
    Q = "UPDATE media SET chunks = chunks + ?, size = ? WHERE id = ?",
    V1 = #{id => FileID,
           chunks => [ChunkID],
           size => NewCommittedSize},
    {ok, _} = wocky_db:query(Context, Q, V1, quorum),

    V2 = #{chunk_id => ChunkID,
           file_id => FileID,
           data => Data},
    ok = wocky_db:insert(Context, media_data, V2),
    ok.

-spec close(francus_file()) -> ok.
close(#state{pending = <<>>}) -> ok;
close(#state{file_id = FileID, committed_size = CS,
             context = Context, pending = Pending}) ->
    commit_chunk(FileID, Context, CS + byte_size(Pending), Pending),
    ok.

%%% READ %%%%

-spec open_read(wocky_db:context(), binary()) ->
    {ok, francus_file()} | not_found.
open_read(Context, FileID) ->
    Row = wocky_db:select_row(Context, media,
                              [content_type, user, size, chunks],
                              #{id => FileID}),
    case Row of
        undefined -> not_found;
        Row -> {ok, open_result(Context, FileID, Row)}
    end.

open_result(Context, FileID,
            Row = #{user := UserID, size := Size,
                    content_type := ContentType}) ->
    maybe_add_chunks(#state{file_id = FileID,
                            user_id = UserID,
                            context = Context,
                            size = Size,
                            content_type = ContentType}, Row).

maybe_add_chunks(State, #{chunks := Chunks}) -> State#state{chunks = Chunks};
% An empty file will have no chunks entry in the map
maybe_add_chunks(State, _) -> State.

-spec read(francus_file()) -> eof | {francus_file(), binary()}.
read(State) -> read(State, infinity).

-spec read(francus_file(), pos_integer() | infinity) ->
    eof | {francus_file(), binary()}.
% End of file reached:
read(#state{chunks = [], pending = <<>>}, _) -> eof;
% Read everything from DB, but still have some to return:
read(S = #state{chunks = [], pending = Pending}, Size)
  when Size =:= infinity orelse byte_size(Pending) =< Size ->
    {S#state{pending = <<>>}, Pending};
% Data still in DB, but enough already read to return requested amount:
read(S = #state{pending = Pending}, Size)
  when is_integer(Size) andalso byte_size(Pending) > Size ->
    <<ToReturn:Size/binary, Remaining/binary>> = Pending,
    {S#state{pending = Remaining}, ToReturn};
% Data still in DB and not enough already read. Do a DB read to get more data
% and then try again:
read(S, Size) ->
    S1 = read_chunk(S),
    read(S1, Size).

read_chunk(S = #state{context = Context, chunks = [Chunk | Rest],
                      pending = Pending}) ->
    NewData = wocky_db:select_one(Context, media_data, data,
                                  #{chunk_id => Chunk}),
    S#state{chunks = Rest, pending = <<Pending/binary, NewData/binary>>}.

%%% DELETE %%%

-spec delete(wocky_db:context(), binary()) -> not_found | ok.
delete(Context, FileID) ->
    case open_read(Context, FileID) of
        not_found ->
            not_found;
        {ok, File} ->
            delete_file(File)
    end.

delete_file(#state{context = Context, file_id = FileID, chunks = Chunks}) ->
    delete_chunks(Context, Chunks),
    delete_metadata(Context, FileID).

delete_chunks(Context, Chunks) ->
    lists:foreach(fun(C) -> delete_chunk(Context, C) end, Chunks).

delete_chunk(Context, Chunk) ->
    wocky_db:delete(Context, media_data, all, #{chunk_id => Chunk}).

delete_metadata(Context, FileID) ->
    wocky_db:delete(Context, media, all, #{id => FileID}).

-spec owner(francus_file()) -> binary().
owner(#state{user_id = Owner}) -> wocky_db_user:normalize_id(Owner).

-spec content_type(francus_file()) -> binary().
content_type(#state{content_type = ContentType}) -> ContentType.

-spec size(francus_file()) -> non_neg_integer().
size(#state{size = Size}) -> Size.

default_chunk_size() -> ?DEFAULT_CHUNK_SIZE.

chunk_size() ->
    case ejabberd_config:get_global_option(francus_chunk_size) of
        X when is_integer(X) -> X;
        _ -> ?DEFAULT_CHUNK_SIZE
    end.
