%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Helper function implementing common metrics operations
%%%
-module(wocky_metrics).

-export([setup_spiral/1, inc/1, inc/2]).

setup_spiral(Metrics) ->
    lists:foreach(fun(M) ->
                          mongoose_metrics:create(
                            [wocky_app:server(), M], spiral)
                  end,
                  Metrics).

inc(Metric) ->
    inc(Metric, 1).

inc(Metric, Count) ->
    mongoose_metrics:update({wocky_app:server(), Metric}, Count).
