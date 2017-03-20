%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module providing traffic dumping and pretty printing
%%%
-module(traffic_dumper).

-compile({parse_transform, do}).
-compile({parse_transform, cut}).

-include("wocky.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").

-define(timex, 'Elixir.Timex').
-define(ansi, 'Elixir.IO.ANSI').

-export([dump/3,
         dump/4]).

-ignore_xref([dump/3,
              dump/4]).

% All user's traffic
dump(Handle, StartBin, DurationBin) ->
    dump(Handle, any, StartBin, DurationBin).

% With resource filter
dump(Handle, Resource, StartBin, DurationBin) ->
    do([error_m ||
        User <- get_user(Handle),
        Start <- get_time(StartBin),
        Duration <- get_duration(DurationBin),
        get_traffic(User, Resource, Start, Duration)
       ]).

get_user(Handle) ->
    case ?wocky_user:search(handle, Handle) of
        [] -> {error, "User not found"};
        [#{id := ID, server := Server}] -> {ok, jid:make(ID, Server, <<>>)}
    end.

get_time(StartBin) ->
    case ?timex:parse(StartBin, <<"{ISO:Extended:Z}">>) of
        {error, Term} -> {error, Term};
        {ok, Result} -> {ok, ?timex:to_unix(Result)}
    end.

get_duration(DurationBin) ->
    case re:split(DurationBin, "([0-9]+)(ms|s|m|h|d|w)", [{return, list}]) of
        [[], Num, Unit, []] -> {ok, apply_unit(list_to_integer(Num), Unit)};
        _ -> {error, "Invalid duration"}
    end.

apply_unit(N, "ms") -> N;
apply_unit(N, "s") -> N * 1000;
apply_unit(N, "m") -> N * 1000 * 60;
apply_unit(N, "h") -> N * 1000 * 60 * 60;
apply_unit(N, "d") -> N * 1000 * 60 * 60 * 24;
apply_unit(N, "w") -> N * 1000 * 60 * 60 * 24 * 7.

get_traffic(User, Resource, Start, Duration) ->
    StartTS = timer:seconds(Start),
    DurationTS = timer:seconds(Duration),
    Q = <<"SELECT * FROM traffic_log WHERE user = ? AND timestamp >= :start "
          "AND timestamp < :stop">>,
    V = #{user => jid:to_binary(User),
          start => StartTS,
          stop => StartTS + DurationTS},
    Result = wocky_db:query(shared, Q, V, one),
    display_result(Result, Resource).

display_result(no_more_results, _) ->
    ok;
display_result({ok, Result}, Resource) ->
    Rows = wocky_db:rows(Result),
    Filtered = lists:filter(show_row(_, Resource), Rows),
    lists:foreach(format_row(_), Filtered),
    display_result(wocky_db:fetch_more(Result), Resource).

format_row(#{user := User, resource := Resource, timestamp := Timestamp,
             ip := IP, incoming := Incoming, server := Server,
             packet := Packet}) ->
    io:fwrite("~s/~s (~s) ~s ~s @ ~s\n~s~s~s\n",
              [User, Resource, IP, direction_arrow(Incoming), Server,
               format_timestamp(Timestamp), colour_direction(Incoming),
               format_packet(Packet), clear_colour()]).

show_row(_, any) -> true;
show_row(#{resource := Resource}, Resource) -> true;
show_row(_, _) -> false.


direction_arrow(true) ->
    "<===";
direction_arrow(false) ->
    "===>".

format_timestamp(Timestamp) ->
    {ok, T} = ?timex:format(?timex:from_unix(Timestamp, milliseconds),
                            <<"{ISO:Extended:Z}">>),
    T.

colour_direction(true) -> ?ansi:red();
colour_direction(false) -> ?ansi:cyan().

clear_colour() -> ?ansi:reset().

format_packet(Packet) ->
    {ok, Parsed} = exml:parse(Packet),
    exml:to_pretty_iolist(Parsed).
