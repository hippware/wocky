%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module providing traffic dumping and pretty printing
%%%
-module(traffic_dumper).

-compile({parse_transform, do}).
-compile({parse_transform, cut}).

-include("wocky.hrl").

-export([dump/3,
         dump/4]).

-define(ansi, 'Elixir.IO.ANSI').


% All user's traffic
dump(Handle, StartBin, DurationBin) ->
    dump(Handle, any, StartBin, DurationBin).

% With resource filter
dump(Handle, Resource, StartBin, DurationBin) ->
    do([error_m ||
        User <- get_user(Handle),
        Start <- get_time(StartBin),
        Duration <- get_duration(DurationBin),
        Traffic <- get_traffic(User, Resource, Start, Duration),
        display_result(Traffic)
       ]).

get_user(Handle) ->
    case ?wocky_repo:get_by(?wocky_user, [{handle, Handle}]) of
        nil -> {error, "User not found"};
        #{id := ID} -> {ok, ID}
    end.

get_time(StartBin) ->
    ?wocky_timestamp:from_string(StartBin).

get_duration(DurationBin) ->
    case re:split(DurationBin, "([0-9]+)(ms|s|m|h|d|w)", [{return, list}]) of
        [[], Num, Unit, []] -> {ok, apply_unit(list_to_integer(Num), Unit)};
        _ -> {error, "Invalid duration"}
    end.

apply_unit(N, "ms") -> ?duration:from_milliseconds(N);
apply_unit(N, "s") -> ?duration:from_seconds(N);
apply_unit(N, "m") -> ?duration:from_minutes(N);
apply_unit(N, "h") -> ?duration:from_hours(N);
apply_unit(N, "d") -> ?duration:from_days(N);
apply_unit(N, "w") -> ?duration:from_weeks(N).

get_traffic(User, any, Start, Duration) ->
    {ok, ?wocky_traffic_log:get_by_period(User, Start, Duration)};
get_traffic(User, Resource, Start, Duration) ->
    {ok, ?wocky_traffic_log:get_by_resource(User, Resource, Start, Duration)}.

display_result(Result) ->
    lists:foreach(format_row(_), Result).

format_row(#{user_id := User, resource := Resource, created_at := Timestamp,
             ip := IP, incoming := Incoming, packet := Packet}) ->
    io:fwrite("~s@~s/~s (~s) ~s ~s @ ~s\n~s~s~s\n",
              [User, wocky_xmpp_app:server(), Resource, IP,
               direction_arrow(Incoming),
               wocky_xmpp_app:server(),
               format_timestamp(Timestamp),
               colour_direction(Incoming),
               format_packet(Packet),
               clear_colour()]).

direction_arrow(true) ->
    "<===";
direction_arrow(false) ->
    "===>".

format_timestamp(Timestamp) ->
    ?wocky_timestamp:to_string(Timestamp).

colour_direction(true) -> ?ansi:red();
colour_direction(false) -> ?ansi:cyan().

clear_colour() -> ?ansi:reset().

format_packet(Packet) ->
    {ok, Parsed} = exml:parse(Packet),
    exml:to_pretty_iolist(Parsed).
