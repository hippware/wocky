%%% @copyright 2017+ Hippware, Inc.
%%%
%%% @doc Module implementing reports on wocky data
%%%
-module(wocky_report).

-compile({parse_transform, cut}).

-include("wocky.hrl").

-export([generate_bot_report/1]).


% Duration is in days
-spec generate_bot_report(non_neg_integer()) -> iolist().
generate_bot_report(Duration) ->
    After = ?timex:to_unix(?timex:now()) - (Duration * 60 * 60 * 24),
    Bots = ?wocky_bot:all(),
    Report = lists:map(maybe_report_bot(_, After), Bots),
    [header(), Report].

header() ->
    "ID,Title,Owner,Created,Updated,Address,Latitude,Longitude,"
    "Visibility,Subscribers,ImageItems,Description\n".

maybe_report_bot(#{created_at := Created} = Bot, After) when Created > After ->
    report_bot(Bot);
maybe_report_bot(_, _) ->
    [].

report_bot(#{id := ID,
             title := Title,
             user_id := OUser,
             address := Address,
             lat := Lat,
             lon := Lon,
             public := Public,
             description := Description,
             created_at := CreatedAt,
             updated_at := UpdatedAt
            } = Bot) ->
    Handle = ?wocky_user:get_handle(OUser),
    io_lib:fwrite("~s,\"~s\",\"~s\",~s,~s,\"~s\",~f,~f,~s,~B,~B,\"~s\"\n",
                  [ID,
                   csv_escape(Title),
                   csv_escape(Handle),
                   ?wocky_timestamp:to_string(CreatedAt),
                   ?wocky_timestamp:to_string(UpdatedAt),
                   csv_escape(Address),
                   Lat, Lon,
                   vis_string(Public),
                   ?wocky_bot:subscriber_count(Bot),
                   ?wocky_bot:image_items_count(Bot),
                   csv_escape(Description)]).

csv_escape(nil) ->
    <<"<nil>">>;
csv_escape(String) ->
    binary:replace(String, <<"\"">>, <<"\"\"">>, [global]).

vis_string(true) -> "public";
vis_string(_) -> "private".
