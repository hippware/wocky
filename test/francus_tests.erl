%%% @copyright 2016+ Hippware, Inc.
%%% @doc Unit tests for Francus C* file store
-module(francus_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/jlib.hrl").

-define(SERVER, "localhost").
-define(CONTENT_TYPE, <<"image/jpeg">>).
-define(NAME, <<"testfile.txt">>).

francus_test_() -> {
  "francus",
  setup, fun before_all/0, fun after_all/1,
  [
    test_read(),
    test_write(),
    test_delete(),
    test_accessors()
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
     10000000
    ].

before_all() ->
    ok = wocky_app:start(),
    ok = wocky_db_seed:prepare_tables(?SERVER, [media, media_data]),
    meck:new(ejabberd_config),
    meck:expect(ejabberd_config, get_global_option,
                fun(francus_chunk_size) -> undefined % Use the default
                end).

after_all(_) ->
    true = meck:validate(ejabberd_config),
    meck:unload(ejabberd_config),
    ok = wocky_app:stop().

make_file(Size) ->
    ID = mod_hxep:make_file_id(),
    Data = crypto:rand_bytes(Size),
    {ID, Data}.

metadata() ->
    #{<<"content-type">> => ?CONTENT_TYPE, <<"name">> => ?NAME}.

write_file(ID, <<>>, User, _, Size, ChunkList) ->
    Chunks = lists:reverse(ChunkList),
    V = #{id => ID, user => User,
          metadata => metadata(),
          chunks => Chunks, size => Size},
    ok = wocky_db:insert(?SERVER, media, V);
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
    ChunkID = mod_hxep:make_file_id(),
    V = #{chunk_id => ChunkID, file_id => FileID, data => Data},
    ok = wocky_db:insert(?SERVER, media_data, V),
    ChunkID.

before_each() ->
    User = wocky_db_user:create_id(),
    ChunkSize = francus:default_chunk_size(),
    Files = [make_file(Size) || Size <- test_sizes()],
    [write_file(ID, Data, User, ChunkSize, byte_size(Data), [])
     || {ID, Data} <- Files],
    #config{user = User, files = Files}.

after_each(_) ->
    wocky_db_seed:clear_tables(?SERVER, [media, media_data]),
    ok.

test_read() ->
    { "francus:read", setup, fun before_each/0, fun after_each/1, fun(Config) ->
      [
       { "Read entire files from the DB",
         [?_test(
             begin
                 {ok, F} = francus:open_read(?SERVER, ID),
                 {F2, ReadData} = francus:read(F),
                 ?assertEqual(Data, ReadData),
                 ?assertEqual(eof, francus:read(F2))
             end) || {ID, Data} <- Config#config.files, Data =/= <<>>] ++
         [?_test(
             begin
                 {ok, F} = francus:open_read(?SERVER, ID),
                 ?assertEqual(eof, francus:read(F))
             end) || {ID, Data} <- Config#config.files, Data =:= <<>>]
       },
       { "Check return value for non-existant files",
         [
          ?_assertEqual(not_found, francus:open_read(?SERVER,
                                                     mod_hxep:make_file_id()))
         ]
       },
       { "Read in smaller chunks",
         [?_test(
             begin
                 {ok, F} = francus:open_read(?SERVER, ID),
                 Result = read_random_chunks(F, <<>>),
                 ?assertEqual(Data, Result)
             end) || {ID, Data} <- Config#config.files, Data =/= <<>>]
       }
      ]end}.

read_random_chunks(F, Acc) ->
    ReadSize = random:uniform(2*francus:default_chunk_size()),
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
       % Don't do setup here - it takes time and we're not going to use the
       % standard files. Still do cleanup though.
       { "Write an entire file", setup, fun() -> ok end , fun after_each/1,
         [?_test(
             begin
                 ID = mod_hxep:make_file_id(),
                 {ok, F} = francus:open_write(?SERVER, ID,
                                              wocky_db_user:create_id(),
                                              metadata()),
                 Data = crypto:rand_bytes(Size),
                 F2 = francus:write(F, Data),
                 ok = francus:close(F2),
                 verify_contents(ID, Data)
             end) || Size <- test_sizes(), Size =/= 0]
       }
      ]
    }.

verify_contents(ID, Data) ->
    {ok, F} = francus:open_read(?SERVER, ID),
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
                 ?assertEqual(ok, francus:delete(?SERVER, ID)),
                 ?assertEqual(not_found, francus:open_read(?SERVER, ID))
             end) || {ID, _} <- Config#config.files]
       },
       { "Non-existant files should still return ok on delete",
         [
          ?_assertEqual(ok, francus:delete(?SERVER, mod_hxep:make_file_id()))
         ]
       },
       { "Check that the DB is properly empty after we deleted everything",
         [
          ?_test(
             begin
                 Q = "SELECT " ++ Key ++ " FROM " ++ Table,
                 {ok, R} = wocky_db:query(?SERVER, Q, [], quorum),
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
      [
       { "Test accessors for existing files",
        [?_test(
            begin
                {ok, F} = francus:open_read(?SERVER, ID),
                #{<<"content-type">> := CT, <<"name">> := Name}
                = francus:metadata(F),
                ?assertEqual(?CONTENT_TYPE, CT),
                ?assertEqual(?NAME, Name),
                ?assertEqual(Config#config.user, francus:owner(F)),
                ?assertEqual(byte_size(Data), francus:size(F))
            end) || {ID, Data} <- Config#config.files
        ]
       },
       { "Test accessors for new file",
         [?_test(
             begin
                 User = wocky_db_user:create_id(),
                 {ok, F} = francus:open_write(?SERVER, mod_hxep:make_file_id(),
                                              User, metadata()),
                 #{<<"content-type">> := CT, <<"name">> := Name}
                 = francus:metadata(F),
                 ?assertEqual(?CONTENT_TYPE, CT),
                 ?assertEqual(?NAME, Name),
                 ?assertEqual(User, francus:owner(F))
             end)
         ]
       }

      ]
      end
    }.


