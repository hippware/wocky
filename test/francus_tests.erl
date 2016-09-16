%%% @copyright 2016+ Hippware, Inc.
%%% @doc Unit tests for Francus C* file store
-module(francus_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include("wocky_db_seed.hrl").

-define(CONTENT_TYPE, <<"image/jpeg">>).
-define(NAME, <<"testfile.txt">>).

francus_test_() -> {
  "francus",
  setup, fun before_all/0, fun after_all/1,
  [
    test_read(),
    test_write(),
    test_delete(),
    test_accessors(),
    test_expire()
  ]
}.

-record(config, {
          user,
          files
         }).

test_sizes() ->
    ChunkSize = francus:default_chunk_size(),
    [
     0,
     1,
     ChunkSize - 1,
     ChunkSize,
     ChunkSize + 1,
     2 * ChunkSize - 1,
     2 * ChunkSize,
     2 * ChunkSize + 1,
     1000000
    ].

before_all() ->
    ok = wocky_db:prepare_tables(?LOCAL_CONTEXT, [media, media_data]).

after_all(_) ->
    ok.

make_file(Size) ->
    ID = mod_wocky_tros:make_file_id(),
    Data = crypto:strong_rand_bytes(Size),
    {ID, Data}.

purpose() -> <<>>.

access() -> <<>>.

metadata() ->
    #{<<"content-type">> => ?CONTENT_TYPE, <<"name">> => ?NAME}.

write_file(ID, <<>>, User, _, Size, ChunkList) ->
    Chunks = lists:reverse(ChunkList),
    V = #{id => ID, user => User,
          metadata => metadata(),
          chunks => Chunks, size => Size},
    ok = wocky_db:insert(?LOCAL_CONTEXT, media, V);
write_file(ID, Data, User, ChunkSize, Size, ChunkList) ->
    {ToWrite, Rest} =
    case ChunkSize of
        X when X > byte_size(Data) ->
            {Data, <<>>};
        _ ->
            <<Write:ChunkSize/binary, Remaining/binary>> = Data,
            {Write, Remaining}
    end,
    write_file(ID, Rest, User, ChunkSize, Size,
               [write_chunk(ID, ToWrite) | ChunkList]).

write_chunk(FileID, Data) ->
    ChunkID = mod_wocky_tros:make_file_id(),
    V = #{chunk_id => ChunkID, file_id => FileID, data => Data},
    ok = wocky_db:insert(?LOCAL_CONTEXT, media_data, V),
    ChunkID.

before_each() ->
    User = wocky_db:create_id(),
    ChunkSize = francus:default_chunk_size(),
    Files = [make_file(Size) || Size <- test_sizes()],
    [write_file(ID, Data, User, ChunkSize, byte_size(Data), [])
     || {ID, Data} <- Files],
    #config{user = User, files = Files}.

after_each(_) ->
    wocky_db:clear_tables(?LOCAL_CONTEXT, [media, media_data]),
    ok.

test_read() ->
    { "francus:read", setup, fun before_each/0, fun after_each/1, fun(Config) ->
     {inparallel, [
       { "Read entire files from the DB", {inparallel,
         [{timeout, 10,
           ?_test(
             begin
                 {ok, F} = francus:open_read(?LOCAL_CONTEXT, ID),
                 {F2, ReadData} = francus:read(F),
                 ?assertEqual(Data, ReadData),
                 ?assertEqual(eof, francus:read(F2))
             end)} || {ID, Data} <- Config#config.files, Data =/= <<>>] ++
         [?_test(
             begin
                 {ok, F} = francus:open_read(?LOCAL_CONTEXT, ID),
                 ?assertEqual(eof, francus:read(F))
             end) || {ID, Data} <- Config#config.files, Data =:= <<>>]}
       },
       { "Check return value for non-existant files",
         [
          ?_assertEqual({error, not_found},
                        francus:open_read(?LOCAL_CONTEXT,
                                          mod_wocky_tros:make_file_id()))
         ]
       },
       { "Read in smaller chunks", {inparallel,
         [?_test(
             begin
                 {ok, F} = francus:open_read(?LOCAL_CONTEXT, ID),
                 Result = read_random_chunks(F, <<>>),
                 ?assertEqual(Data, Result)
             end) || {ID, Data} <- Config#config.files, Data =/= <<>>]}
       }
      ]} end}.

read_random_chunks(F, Acc) ->
    ReadSize = rand:uniform(2*francus:default_chunk_size()),
    case francus:read(F, ReadSize) of
        {F2, Data} when byte_size(Data) =:= ReadSize ->
            read_random_chunks(F2, <<Acc/binary, Data/binary>>);
        {F2, Data} when byte_size(Data) < ReadSize ->
            read_eof(F2, <<Acc/binary, Data/binary>>);
        eof ->
            Acc
    end.

read_eof(F, Acc) ->
    ?assertEqual(eof, francus:read(F)),
    Acc.


test_write() ->
    { "francus:write",
      [
       %% Don't do setup here - it takes time and we're not going to use the
       %% standard files. Still do cleanup though.
       { "Write an entire file", setup, fun() -> ok end , fun after_each/1,
         {inparallel,
          {timeout, 10,
            [
             ?_test(
               begin
                   ID = mod_wocky_tros:make_file_id(),
                   {ok, F} = francus:open_write(?LOCAL_CONTEXT, ID,
                                                wocky_db:create_id(),
                                                purpose(), access(),
                                                metadata()),
                   Data = crypto:strong_rand_bytes(Size),
                   F2 = francus:write(F, Data),
                   ok = francus:close(F2),
                   verify_contents(ID, Data)
               end) || Size <- test_sizes(), Size =/= 0]}
         }
       }
      ]
    }.

test_expire() ->
    { "francus expiry", {inparallel,
      [
       { "should clear a file after the expiry period",
         [?_test(
             begin
                 ID = create_small_file(1),
                 timer:sleep(timer:seconds(3)),
                 ?assertEqual({error, not_found},
                              francus:open_read(?LOCAL_CONTEXT, ID))
             end
            )]
       },
       { "should not clear a file that's been accessed",
         [?_test(
             begin
                 ID = create_small_file(1),
                 {ok, F} = francus:open_read(?LOCAL_CONTEXT, ID),
                 francus:close(F),
                 timer:sleep(timer:seconds(3)),
                 ?assertMatch({ok, _},
                              francus:open_read(?LOCAL_CONTEXT, ID))
             end
            )]
       },
       { "should keep a previously expiring file",
         [?_test(
            begin
                ID = create_small_file(2),
                francus:keep(?LOCAL_CONTEXT, ID),
                timer:sleep(timer:seconds(3)),
                ?assert(is_tuple(francus:open_read(?LOCAL_CONTEXT, ID)))
            end
           )]
       }
      ]}}.


verify_contents(ID, Data) ->
    {ok, F} = francus:open_read(?LOCAL_CONTEXT, ID),
    {F2, ReadData} = francus:read(F),
    ?assertEqual(eof, francus:read(F2)),
    ?assertEqual(Data, ReadData).

test_delete() ->
    { "francus:delete", setup, fun before_each/0, fun after_each/1,
      fun(Config) ->
      [
       { "Delete all files",
         [?_test(
             begin
                 ?assertEqual(ok, francus:delete(?LOCAL_CONTEXT, ID)),
                 ?assertEqual({error, not_found},
                              francus:open_read(?LOCAL_CONTEXT, ID))
             end) || {ID, _} <- Config#config.files]
       },
       { "Non-existant files should still return ok on delete",
         [
          ?_assertEqual(ok, francus:delete(?LOCAL_CONTEXT,
                                           mod_wocky_tros:make_file_id()))
         ]
       },
       { "Check that the DB is properly empty after we deleted everything",
         [
          ?_test(
             begin
                 Q = "SELECT " ++ Key ++ " FROM " ++ Table,
                 {ok, R} = wocky_db:query(?LOCAL_CONTEXT, Q, #{}, quorum),
                 ?assertEqual([], wocky_db:rows(R))
             end) || {Key, Table} <-
                     [{"id", "media"}, {"chunk_id", "media_data"}]
         ]
       }
      ]
      end
    }.

test_accessors() ->
    { "Francus accessors", setup, fun before_each/0, fun after_each/1,
      fun(Config) ->
      {inparallel, [
       { "Test accessors for existing files",
         {inparallel,
          [?_test(
            begin
                {ok, F} = francus:open_read(?LOCAL_CONTEXT, ID),
                #{<<"content-type">> := CT, <<"name">> := Name}
                = francus:metadata(F),
                ?assertEqual(?CONTENT_TYPE, CT),
                ?assertEqual(?NAME, Name),
                ?assertEqual(Config#config.user, francus:owner(F)),
                ?assertEqual(byte_size(Data), francus:size(F))
            end) || {ID, Data} <- Config#config.files
          ]}
       },
       { "Test accessors for new file",
         [?_test(
             begin
                 User = wocky_db:create_id(),
                 {ok, F} = francus:open_write(?LOCAL_CONTEXT,
                                              mod_wocky_tros:make_file_id(),
                                              User,
                                              purpose(), access(), metadata()),
                 #{<<"content-type">> := CT, <<"name">> := Name}
                 = francus:metadata(F),
                 ?assertEqual(?CONTENT_TYPE, CT),
                 ?assertEqual(?NAME, Name),
                 ?assertEqual(User, francus:owner(F))
             end)
         ]
       }
      ]}
      end
    }.


create_small_file(TTL) ->
    ID = mod_wocky_tros:make_file_id(),
    {ok, F} = francus:open_write(?LOCAL_CONTEXT, ID,
                                 wocky_db:create_id(),
                                 purpose(), access(),
                                 metadata(), TTL),
    F1 = francus:write(F, <<"abc">>),
    francus:close(F1),
    ID.
