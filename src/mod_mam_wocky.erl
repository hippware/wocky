%%% @copyright 2016+ Hippware, Inc.
%%% @doc Wocky C* Backend for mod_mam
%%%
-module(mod_mam_wocky).

-behaviour(gen_mod).

-include_lib("ejabberd/include/jlib.hrl").

%% gen_mod handlers
-export([start/2, stop/1]).

%% MAM hook handlers
-export([
         archive_message/9,
         lookup_messages/14,
         jid_key/2
        ]).

-ifdef(TEST).
-export([
         archive_test_message/7
        ]).
-endif.

%% Query construction internal exports
-export([add_jids/3,
         add_times/3,
         add_ordering/2,
         add_limit/2]).

-type result_row() :: {non_neg_integer(), ejabberd:jid(), exml:element()}.

-define(HOOK_SEQ, 50).

hooks(Host) ->
    [
     [mam_archive_message, Host, ?MODULE, archive_message, ?HOOK_SEQ],
     [mam_lookup_messages, Host, ?MODULE, lookup_messages, ?HOOK_SEQ]
    ].

start(Host, _Opts) ->
    hooks_op(Host, add),
    ok.

stop(Host) ->
    hooks_op(Host, delete),
    ok.

hooks_op(Host, Op) ->
    lists:foreach(fun(H) -> apply(ejabberd_hooks, Op, H) end, hooks(Host)).

-spec archive_message(Result :: any(),
                      Host   :: ejabberd:server(),
                      MessID :: mod_mam:message_id(),
                      ArcID  :: mod_mam:archive_id(),
                      LocJID :: ejabberd:jid(),
                      RemJID :: ejabberd:jid(),
                      SrcJID :: ejabberd:jid(),
                      Dir    :: incoming | outgoing,
                      Packet :: exml:element()
                     ) -> ok.
archive_message(_Result, Host, MessID, _UserID,
                LocJID, RemJID, _SrcJID, incoming, Packet) ->
    TTL = gen_mod:get_module_opt(global, ?MODULE, message_ttl, infinity),
    PartKey = jid_key(LocJID, RemJID),
    PacketBin = exml:to_binary(Packet),
    ToLower = sent_to_lower(LocJID, RemJID),
    Row = PartKey#{id => MessID, time => now, message => PacketBin,
                   sent_to_lower => ToLower},
    ok = wocky_db:insert(Host, message_archive, maybe_add_ttl(Row, TTL));

archive_message(_Result, _Host, _MessID, _UserID,
                _LocJID, _RemJID, _SrcJID, outgoing, _Packet) ->
    %% Will be archived by remote jid.
    ok.

-spec lookup_messages(
        Result         :: term(),
        Host           :: ejabberd:server(),
        UserID         :: mod_mam:archive_id(),
        UserJID        :: ejabberd:jid(),
        RSM            :: jlib:rsm_in() | undefined,
        Borders        :: mod_mam:borders() | undefined,
        Start          :: mod_mam:unix_timestamp() | undefined,
        End            :: mod_mam:unix_timestamp() | undefined,
        Now            :: mod_mam:unix_timestamp(),
        WithJID        :: ejabberd:jid() | undefined,
        PageSize       :: non_neg_integer(),
        LimitPassed    :: boolean(),
        MaxResultLimit :: non_neg_integer(),
        IsSimple       :: boolean()
       ) ->
    {ok, {TotalCount  :: non_neg_integer() | undefined,
          Offset      :: non_neg_integer() | undefined,
          MessageRows :: [result_row()]}}
    | {error, missing_with_jid}.

%% No second JID - not implemented nor expected to be:
lookup_messages(_, _, _, _, _, _, _, _, _, undefined, _, _, _, _) ->
    {error, missing_with_jid};

%% No RSM data, no borders; time only - generate some RSM data and use the
%% function below:
lookup_messages(Result, Host, UserID, UserJID,
                undefined, undefined,
                Start, End, Now, WithJID,
                PageSize, LimitPassed, MaxResultLimit, Simple) ->
    lookup_messages(Result, Host, UserID, UserJID,
                    #rsm_in{max = PageSize}, undefined, Start, End, Now,
                    WithJID, PageSize, LimitPassed, MaxResultLimit, Simple);

lookup_messages(_Result, Host, _UserID, UserJID,
                #rsm_in{id = undefined, index = undefined,
                        max = RSMMax, direction = Direction},
                undefined, Start, End, _Now, WithJID,
                RSMMax, _LimitPassed, _MaxResultLimit, Simple) ->
    TaggedStart = {time, Start},
    TaggedEnd = {time, End},
    Rows = do_lookup(Host, UserJID, WithJID, TaggedStart, TaggedEnd,
                     RSMMax, Direction),
    Counts = standard_counts(Simple, RSMMax, Host, UserJID, WithJID,
                             TaggedStart, TaggedEnd, undefined, Rows),
    return_result(Counts, Rows);

%% No RSM data, borders present. Generate RSM based off the pagesize and use
%% the function below:
lookup_messages(Result, Host, UserID, UserJID,
                undefined, Borders,
                undefined, undefined, Now, WithJID,
                PageSize, LimitPassed, MaxResultLimit, Simple) ->
    lookup_messages(Result, Host, UserID, UserJID,
                #rsm_in{max = PageSize}, Borders, undefined, undefined,
                Now, WithJID, PageSize, LimitPassed, MaxResultLimit, Simple);

lookup_messages(_Result, Host, _UserID, UserJID,
                #rsm_in{
                   max = RSMMax,
                   direction = Direction,
                   index = undefined,
                   id = undefined
                  },
                Borders =
                #mam_borders{after_id = AfterID,
                             before_id = BeforeID,
                             from_id = FromID,
                             to_id = ToID
                            },
                undefined, undefined, _Now, WithJID,
                RSMMax, _LimitPassed, _MaxResultLimit, Simple) ->
    TaggedStart = get_time_from_id(Host, UserJID, WithJID, AfterID, FromID),
    TaggedEnd = get_time_from_id(Host, UserJID, WithJID, BeforeID, ToID),
    Rows = do_lookup(Host, UserJID, WithJID, TaggedStart, TaggedEnd,
                     RSMMax, Direction),
    Counts = standard_counts(Simple, RSMMax, Host, UserJID, WithJID,
                             TaggedStart, TaggedEnd, Borders, Rows),
    return_result(Counts, Rows);

%% RSM data present with index only (out-of-order retrieval):
lookup_messages(_Result, Host, _UserID, UserJID,
                #rsm_in{id = undefined, index = Index,
                        max = RSMMax, direction = Direction},
                undefined, undefined, undefined, _Now, WithJID,
                RSMMax, _LimitPassed, _MaxResultLimit, Simple) ->
    {Q, V} = find_nth_query_val(UserJID, WithJID, Index, RSMMax, Direction),
    Rows = run_paging_query(Host, Q, V),
    SplitPoint = min(length(Rows), Index),
    {_, RequestedRows} = lists:split(SplitPoint, Rows),
    Counts = index_only_counts(Simple, RSMMax, Host, UserJID, WithJID,
                               SplitPoint),
    return_result(Counts, RequestedRows);

%% RSM data present with ID - find the timestamp for that ID and do a timestamp
%% based lookup:
lookup_messages(_Result, Host, _UserID, UserJID,
                #rsm_in{direction = Direction, id = ID,
                        index = undefined, max = RSMMax},
                undefined, undefined, undefined, _Now, WithJID,
                RSMMax, _LimitPassed, _MaxResultLimit, Simple) ->
    StartPoint = get_time_from_id(Host, UserJID, WithJID, ID, undefined),
    TaggedStart = make_start(Direction, StartPoint),
    TaggedEnd = make_end(Direction, StartPoint),
    Rows = do_lookup(Host, UserJID, WithJID, TaggedStart, TaggedEnd,
                     RSMMax, Direction),
    Counts = standard_counts(Simple, RSMMax, Host, UserJID, WithJID,
                             undefined, undefined, undefined, Rows),
    return_result(Counts, Rows);

lookup_messages(_, _, _, _, _, _, _, _, _, _, _, _, _, _) ->
    error(unhandled_lookup_parameters).

do_lookup(_Host, _UserJID, _WithJID, _Start, _End, 0, _Direction) ->
    [];

do_lookup(Host, JID1, JID2, Start, End, Max, Direction) ->
    InitialQuery = "SELECT * FROM message_archive WHERE ",
    {Query, Values} =
    lists:foldl(fun({F, A}, {Q, V}) -> apply(?MODULE, F, [{Q, V} | A]) end,
                {InitialQuery, #{}},
                [{add_jids,     [JID1, JID2]},
                 {add_times,    [Start, End]},
                 {add_ordering, [Direction]},
                 {add_limit,    [Max]}]),
    run_paging_query(Host, Query, Values).

return_result({Total, OffsetCount}, Rows) ->
    {ok, {Total, OffsetCount, lists:sort([row_to_msg(R) || R <- Rows])}}.

make_start(before, _) -> undefined;
make_start(aft, Time) -> Time.

make_end(before, Time) -> Time;
make_end(aft, _) -> undefined.

add_jids({Q, V}, UserJID, WithJID) ->
    {[Q, " lower_jid = ? AND upper_jid = ?"],
     maps:merge(V, jid_key(UserJID, WithJID))}.

get_time_from_id(_Host, _, _, undefined, undefined) -> undefined;
get_time_from_id(Host, JID1, JID2, Exclusive, undefined) ->
    {uuid, exclusive, get_time_from_id(Host, JID1, JID2, Exclusive)};
get_time_from_id(Host, JID1, JID2, undefined, Inclusive) ->
    {uuid, inclusive, get_time_from_id(Host, JID1, JID2, Inclusive)}.

get_time_from_id(Host, JID1, JID2, ID) ->
    PartKey = jid_key(JID1, JID2),
    wocky_db:select_one(Host, archive_id, time, PartKey#{id => ID}).

add_borders({Q, V},
            #mam_borders{after_id = AfterID, before_id = BeforeID,
                         from_id = FromID, to_id = ToID}) ->
    lists:foldl(fun({B, Op}, Acc) -> add_border(Acc, B, Op) end,
                {Q, V},
                [{AfterID, ">"},
                 {BeforeID, "<"},
                 {FromID, ">="},
                 {ToID, "<="}]).


add_border({Q, V}, undefined, _Op) -> {Q, V};
add_border({Q, V}, Border, Op) ->
    {BindStr, BindAtom} = make_binding("border", Op),
    {[Q, " AND id ", Op, " ", BindStr], V#{BindAtom => Border}}.

add_times({Q, V}, Start, End) ->
    lists:foldl(fun({Time, Op}, Acc) -> add_time(Acc, Time, Op) end,
                {Q, V},
                [{Start, ">"},
                 {End, "<"}]).

add_time({Q, V}, undefined, _) -> {Q, V};
add_time({Q, V}, {_, undefined}, _) -> {Q, V};
add_time({Q, V}, {time, Time}, Op) ->
    {BindStr, BindAtom} = make_binding("time", Op),
    {UUIDFun, CompareTS} = uuid_fun(Op, mam_to_wocky_ts(Time)),
    {[Q, " AND time ", Op, " ", UUIDFun, "(", BindStr, ")"],
     V#{BindAtom => CompareTS}};
add_time({Q, V}, {uuid, Type, Time}, Op) ->
    {BindStr, BindAtom} = make_binding("time_uuid", Op),
    AllowMatch = maybe_inclusive(Type),
    {[Q, " AND time ", Op, AllowMatch, " ", BindStr],
     V#{BindAtom => Time}}.

maybe_inclusive(inclusive) -> "=";
maybe_inclusive(exclusive) -> "".

% mod_mam uses microsecd timestamps; wocky_db uses millisecond
mam_to_wocky_ts(TimeUS) ->
    TimeUS div 1000.

uuid_fun(">", Time) ->
    {"maxTimeuuid", Time-1};
uuid_fun("<", Time) ->
    {"minTimeuuid", Time+1}.

add_ordering({Q, V}, before) -> {[Q, " ORDER BY time DESC"], V};
add_ordering({Q, V}, _) -> {Q, V}.

add_limit({Q, V}, undefined) -> {Q, V};
add_limit({Q, V}, Limit) ->
    {[Q, " LIMIT ?"], V#{'[limit]' => Limit}}.

jid_key(JID1 = #jid{}, JID2 = #jid{}) ->
    jid_key(archive_jid(JID1), archive_jid(JID2));
jid_key(JID1, JID2) ->
    [Lower, Higher] = lists:sort([JID1, JID2]),
    #{lower_jid => Lower,
      upper_jid => Higher}.

archive_jid(JID) -> jid:to_binary(jid:to_bare(JID)).

sent_to_lower(Receiver, Sender) ->
    archive_jid(Receiver) < archive_jid(Sender).

maybe_add_ttl(Row, infinity) -> Row;
maybe_add_ttl(Row, TTL) -> Row#{'[ttl]' => TTL}.

need_count(_, 0) -> true;
need_count(true, _) -> false;
need_count(false, _) -> true;
need_count(opt_count, _) -> true.

total_count(Host, JID1, JID2, FirstTime, LastTime) ->
    InitialQuery = "SELECT COUNT(*) FROM message_archive WHERE ",
    InitialValues = #{},
    {Query, Values} =
    lists:foldl(fun({F, A}, {Q, V}) -> apply(?MODULE, F, [{Q, V} | A]) end,
                {InitialQuery, InitialValues},
                [{add_jids, [JID1, JID2]},
                 {add_times, [FirstTime, LastTime]}]),
    {ok, Result} = wocky_db:query(Host, Query, Values, quorum),
    wocky_db:single_result(Result).

offset_count(_, _, _, _, []) -> undefined;
offset_count(Host, JID1, JID2, undefined, Rows) ->
    offset_count(Host, JID1, JID2, #mam_borders{}, Rows);
offset_count(Host, JID1, JID2,
             Borders = #mam_borders{before_id = BeforeID}, Rows) ->
    IndexID = index_id(Rows),
    NewBorders = Borders#mam_borders{before_id = min(IndexID, BeforeID),
                                     to_id = undefined},
    offset_count(Host, JID1, JID2, NewBorders).

offset_count(Host, JID1, JID2, Borders) ->
    BaseQuery = ["SELECT COUNT(*) FROM archive_id WHERE lower_jid = ? AND
            upper_jid = ?"],
    PartKey = jid_key(JID1, JID2),
    {Q, V} = add_borders({BaseQuery, PartKey}, Borders),
    {ok, Result} = wocky_db:query(Host, Q, V, quorum),
    wocky_db:single_result(Result).

index_only_counts(CountType, RSMMax, Host, JID1, JID2, Index) ->
    NeedCount = need_count(CountType, RSMMax),
    index_only_counts(NeedCount, Host, JID1, JID2, Index).

index_only_counts(false, _, _, _, _) -> {undefined, undefined};
index_only_counts(true, Host, JID1, JID2, Index) ->
    Total = wocky_db:count(Host, message_archive, jid_key(JID1, JID2)),
    {Total, Index}.

standard_counts(CountType, RSMMax, Host, JID1, JID2, TaggedStart, TaggedEnd,
                Borders, Rows) ->
    NeedCount = need_count(CountType, RSMMax),
    standard_counts(NeedCount, Host, JID1, JID2, TaggedStart, TaggedEnd,
                    Borders, Rows).

standard_counts(false, _, _, _, _, _, _, _) -> {undefined, undefined};
standard_counts(true, Host, JID1, JID2, TaggedStart,
                TaggedEnd, Borders, Rows) ->
    Total = total_count(Host, JID1, JID2, TaggedStart, TaggedEnd),
    OffsetCount = offset_count(Host, JID1, JID2, Borders, Rows),
    {Total, OffsetCount}.

index_id(Rows) ->
    #{id := FirstID} = hd(Rows),
    #{id := LastID} = lists:last(Rows),
    min(FirstID, LastID).

find_nth_query_val(JID1, JID2, Index, Max, Direction) ->
    Q = "SELECT * FROM message_archive WHERE lower_jid = ? AND upper_jid = ?",
    V = jid_key(JID1, JID2),
    {Q2, V2} = add_limit({Q, V}, maybe_add(Index, Max)),
    add_ordering({Q2, V2}, Direction).

row_to_msg(#{id := ID, sent_to_lower := true,
             upper_jid := SrcJID, message := Packet}) ->
    format_msg(ID, SrcJID, Packet);
row_to_msg(#{id := ID, sent_to_lower := false,
             lower_jid := SrcJID, message := Packet}) ->
    format_msg(ID, SrcJID, Packet).

format_msg(ID, SrcJID, Packet) ->
    {ok, XML} = exml:parse(Packet),
    {ID, jid:from_binary(SrcJID), XML}.

make_binding(Prefix, ">") -> make_binding(Prefix, "gt");
make_binding(Prefix, "<") -> make_binding(Prefix, "lt");
make_binding(Prefix, ">=") -> make_binding(Prefix, "gte");
make_binding(Prefix, "<=") -> make_binding(Prefix, "lte");
make_binding(Prefix, Suffix) ->
    Str = Prefix ++ "_" ++ Suffix,
    {[$: | Str], list_to_atom(Str)}.

maybe_add(undefined, _) -> undefined;
maybe_add(_, undefined) -> undefined;
maybe_add(A, B) -> A + B.

run_paging_query(Host, Query, Values) ->
    Result = wocky_db:query(Host, Query, Values, quorum),
    continue_paging_query(Result, []).

continue_paging_query(no_more_result, Acc) -> Acc;
continue_paging_query({ok, Result}, Acc) ->
    Rows = wocky_db:rows(Result),
    NextResult = wocky_db:fetch_more(Result),
    continue_paging_query(NextResult, Acc ++ Rows).


-ifdef(TEST).
archive_test_message(Host, MessID, LocJID, RemJID, Dir, Packet, Timestamp) ->
    Q = "INSERT INTO message_archive (id, lower_jid, upper_jid, time,
         sent_to_lower, message) VALUES (?, ?, ?, minTimeuuid(:time), ?, ?)",
    PartKey = jid_key(LocJID, RemJID),
    SentToLower = sent_to_lower(LocJID, RemJID) xor (Dir =:= outgoing),
    V = PartKey#{id => MessID, message => exml:to_binary(Packet),
                 time => Timestamp, sent_to_lower => SentToLower},
    {ok, void} = wocky_db:query(Host, Q, V, quorum),
    ok.
-endif.

