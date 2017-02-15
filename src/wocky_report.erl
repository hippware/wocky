%%% @copyright 2017+ Hippware, Inc.
%%%
%%% @doc Module implementing reports on wocky data
%%%
-module(wocky_report).

-include("wocky_bot.hrl").

-export([generate_bot_report/1]).

-compile({parse_transform, cut}).

-ignore_xref([generate_bot_report/1]).

-define(timex, 'Elixir.Timex').

% Duration is in days
-spec generate_bot_report(non_neg_integer()) -> iolist().
generate_bot_report(Duration) ->
    Bots = wocky_db:select_column(shared, bot, id, #{}),
    Report = lists:map(maybe_report_bot(_, Duration), Bots),
    [header(), Report].

header() ->
    "ID,Title,Created,Updated,Address,Latitude,Longditude,"
    "Visibility,Subscribers,ImageItems,Description\n".

maybe_report_bot(BotID, Duration) ->
    CreatedAt = uuid:get_v1_time(uuid:string_to_uuid(BotID)) div (1000 * 1000),
    After = ?timex:to_unix(?timex:now()) - (Duration * 60 * 60 * 24),
    case CreatedAt > After of
        true ->
            report_bot(BotID, CreatedAt);
        false ->
            []
    end.

report_bot(BotID, CreatedAt) when is_binary(BotID) ->
    case wocky_db_bot:get_bot(wocky_app:server(), BotID) of
        not_found -> [];
        Bot -> report_bot(Bot, CreatedAt)
    end;

report_bot(#{id := ID,
             server := Server,
             title := Title,
             address := Address,
             lat := Lat,
             lon := Lon,
             visibility := Visibility,
             description := Description,
             updated := Updated
            },
           CreatedAt) ->

    io_lib:fwrite("~s,\"~s\",~s,~s,\"~s\",~f,~f,~s,~B,~B,\"~s\"\n",
                  [ID,
                   csv_escape(Title),
                   time_string(CreatedAt),
                   time_string(wocky_db:timestamp_to_seconds(Updated)),
                   csv_escape(Address),
                   Lat, Lon,
                   vis_string(Visibility),
                   wocky_db_bot:subscriber_count(Server, ID),
                   wocky_db_bot:image_items_count(Server, ID),
                   csv_escape(Description)]).

csv_escape(String) ->
    binary:replace(String, <<"\"">>, <<"\"\"">>, [global]).

vis_string(?WOCKY_BOT_VIS_OWNER) -> "owner";
vis_string(?WOCKY_BOT_VIS_WHITELIST) -> "whitelist";
vis_string(?WOCKY_BOT_VIS_FRIENDS) -> "friends";
vis_string(?WOCKY_BOT_VIS_FOLLOWERS) -> "followers";
vis_string(?WOCKY_BOT_VIS_OPEN) -> "open";
vis_string(_) -> "<unknown>".

time_string(Seconds) ->
    {ok, S} = ?timex:format(?timex:from_unix(Seconds), <<"{ISO:Extended:Z}">>),
    S.
