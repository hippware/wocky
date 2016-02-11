%%% @copyright 2015+ Hippware, Inc.
%%% @doc A module to store XEP-0012 Last Activity data into Cassandra

-module(mod_last_wocky).

-behaviour(mod_last).

-export([init/2,
         get_last/2,
         count_active_users/2,
         set_last_info/4,
         remove_user/2]).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/mod_last.hrl").

%% @doc Initialise this module (from {@link mod_last})
-spec init(ejabberd:server(), list()) -> ok.
init(_Host, _Opts) ->
    ok.


-spec get_last(ejabberd:luser(), ejabberd:lserver())
              -> {ok, non_neg_integer(), string()} | not_found.
get_last(LUser, LServer) ->
    Value = wocky_db:select_row(LServer, last_activity, [timestamp, status],
                                #{user => LUser}),
    case Value of
        #{timestamp := TS, status := S} ->
            {ok, wocky_db:timestamp_to_seconds(TS), binary_to_list(S)};

        not_found ->
            not_found
    end.


-spec count_active_users(ejabberd:lserver(), non_neg_integer())
                        -> non_neg_integer().
count_active_users(LServer, TimeStamp) ->
    Rows = wocky_db:select(LServer, last_activity, [timestamp], #{}),
    Pred = fun(#{timestamp := Val}) ->
                   wocky_db:timestamp_to_seconds(Val) > TimeStamp end,
    length(lists:filter(Pred, Rows)).


-spec set_last_info(ejabberd:luser(), ejabberd:lserver(),
                    non_neg_integer(), binary()) -> ok.
set_last_info(LUser, LServer, TimeStamp, Status) ->
    Values = #{user => LUser, server => LServer, status => Status,
               timestamp => wocky_db:seconds_to_timestamp(TimeStamp)},
    wocky_db:insert(LServer, last_activity, Values).


-spec remove_user(ejabberd:luser(), ejabberd:lserver()) -> ok.
remove_user(LUser, LServer) ->
    wocky_db:delete(LServer, last_activity, all, #{user => LUser}).
