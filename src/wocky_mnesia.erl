%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module to assist with common mnesia operations
%%%
-module(wocky_mnesia).

-export([initialise_shared_ram_table/3]).

-define(WFT_TIMEOUT, timer:seconds(30)).

initialise_shared_ram_table(Name, Opts, Attributes) ->
    case create_shared_ram_table(Name, Opts, Attributes) of
        {aborted, {already_exists, Name}} ->
            mnesia:add_table_copy(Name, node(), ram_copies),
%            ok = mnesia:wait_for_tables([Name], ?WFT_TIMEOUT),
%            transform_table(Name, Attributes);
            ok;
        {atomic, ok} ->
            ok
    end.

create_shared_ram_table(Name, Opts, Attributes) ->
    mnesia:create_table(Name,
                        [{ram_copies, [node()]},
                         {attributes, Attributes}
                         | Opts]).

%transform_table(Name, Attributes) ->
    %% We can skip the transformation function because no nodes should
    %% be running with the old record structure, which for ram tables means
    %% there should be no old records remaining
%    {atomic, ok} = mnesia:transform_table(Name, ignore, Attributes),
%    ok.
