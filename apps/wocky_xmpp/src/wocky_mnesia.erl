%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module to assist with common mnesia operations
%%%
-module(wocky_mnesia).

-export([initialise_shared_ram_table/3]).

-define(WFT_TIMEOUT, timer:seconds(30)).

-spec initialise_shared_ram_table(atom(), proplists:proplist(), [atom()]) -> ok.
initialise_shared_ram_table(Name, Opts, Attributes) ->
    case create_shared_ram_table(Name, Opts, Attributes) of
        {aborted, {already_exists, Name}} ->
            mnesia:add_table_copy(Name, node(), ram_copies),
            ok = mnesia:wait_for_tables([Name], ?WFT_TIMEOUT),
            transform_table(Name, Attributes);
        {atomic, ok} ->
            ok
    end.

create_shared_ram_table(Name, Opts, Attributes) ->
    mnesia:create_table(Name,
                        [{ram_copies, [node()]},
                         {attributes, Attributes}
                         | Opts]).

transform_table(Name, Attributes) ->
    %% We can skip the transformation function because no nodes should
    %% be running with the old record structure, which for ram tables means
    %% there should be no old records remaining
    Result = mnesia:transform_table(Name, ignore, Attributes),

    % The not_active case occurs when not all mnesia nodes have started yet.
    % That's fine - when a later one starts it will also run the transform
    % eventually one or more will get the ok.
    case Result of
        {atomic, ok} -> ok;
        {aborted, {not_active, _, _, _}} -> ok;
        E -> error(E)
    end.
