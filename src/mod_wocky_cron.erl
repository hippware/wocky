%%% @copyright 2017+ Hippware, Inc.
%%%
%%% @doc Module implementing Wocky Cron tasks
%%%
-module(mod_wocky_cron).

-behaviour(gen_mod).

-compile({parse_transform, cut}).

%% gen_mod handlers
-export([start/2, stop/1]).

-define(CRON_TABLE, wocky_cron_procs).

%%%===================================================================
%%% gen_mod handlers
%%%===================================================================

start(Host, Opts) ->
    _ = ets:new(?CRON_TABLE, [named_table, public]),
    Tasks = make_tasks(binary_to_list(Host), Opts),
    Procs = crone:start(Tasks),
    ets:insert(?CRON_TABLE, {procs, Procs}).

stop(_Host) ->
    [{procs, Procs}] = ets:lookup(?CRON_TABLE, procs),
    crone:stop(Procs).

%%%===================================================================
%%% Helpers
%%%===================================================================

make_tasks(Host, Opts) ->
    lists:foldl(add_task(Host, _, _), [], Opts).

add_task(Host, {Host, Task}, Acc) ->
    [Task | Acc];
add_task(_, _, Acc) ->
    Acc.
