%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module implementing Wocky bot notes
%%% See https://github.com/hippware/tr-wiki/wiki/Publishing-format
%%%
-module(bot_notes).

-compile({parse_transform, do}).
-compile({parse_transform, cut}).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").
-include("wocky.hrl").

-export([handle_retrieve/4,
         handle_publish/4,
         handle_retract/4]).

handle_retrieve(From, #jid{lserver = LServer}, IQ, Attrs) ->
    do([error_m ||
        BotID <- bot_utils:get_id_from_node(Attrs),
        bot_utils:check_access(LServer, BotID, From),
        RSMIn <- get_rsm(IQ),
        {Notes, RSMOut} <- get_notes(LServer, BotID, RSMIn),
        {ok, make_results(Notes, RSMOut)}
       ]).

handle_retract(From, To = #jid{lserver = LServer}, SubEl, Attrs) ->
    do([error_m ||
        BotID <- bot_utils:get_id_from_node(Attrs),
        bot_utils:check_owner(LServer, BotID, From),
        Item <- wocky_xml:get_sub_el(<<"item">>, SubEl),
        NoteID <- wocky_xml:get_attr(<<"id">>, Item),
        retract_note(To, BotID, NoteID),
        {ok, []}
       ]).

handle_publish(From, To = #jid{lserver = LServer}, SubEl, Attrs) ->
    do([error_m ||
        BotID <- bot_utils:get_id_from_node(Attrs),
        bot_utils:check_owner(LServer, BotID, From),
        Item <- wocky_xml:get_sub_el(<<"item">>, SubEl),
        NoteID <- wocky_xml:get_attr(<<"id">>, Item#xmlel.attrs),
        Entry <- wocky_xml:get_sub_el(<<"entry">>, Item),
        wocky_xml:check_namespace(?NS_ATOM, Entry),
        Title <- wocky_xml:get_subel_cdata(<<"title">>, Entry),
        Content <- get_content(Entry),
        Media <- get_media(Entry),
        publish_note(To, BotID, NoteID, Title, Content, Media),
        {ok, []}
       ]).

get_content(El) ->
    {ok, xml:get_path_s(El, [{elem, <<"content">>}, cdata])}.

get_media(El) ->
    % Currently we only support the image media type
    case xml:get_path_s(El, [{elem, <<"image">>}]) of
        El = #xmlel{} -> {ok, exml:to_binary(El)};
        <<>> -> {ok, <<>>}
    end.

get_rsm(IQ) ->
    case jlib:rsm_decode(IQ) of
        none -> {error, ?ERRT_BAD_REQUEST(
                           ?MYLANG, <<"Missing or invalid RSM values">>)};
        RSM = #rsm_in{} -> {ok, RSM}
    end.

publish_note(From = #jid{lserver = LServer}, BotID,
             NoteID, Title, Content, Media) ->
    wocky_db_bot:publish_note(LServer, BotID, NoteID, Title, Content, Media),
    Note = wocky_db_bot:get_note(LServer, BotID, NoteID),
    Message = notification_message(BotID, [make_note_item(Note)]),
    notify_subscribers(From, BotID, Message).

notify_subscribers(From = #jid{lserver = LServer}, BotID, Message) ->
    Subscribers = wocky_db_bot:subscribers(LServer, BotID),
    {SubscriberJIDs, _FollowStatuses} = lists:unzip(Subscribers),
    lists:foreach(notify_subscriber(From, _, Message), SubscriberJIDs).

make_note_item(Note) ->
    #xmlel{name = <<"item">>,
           children = [make_entry_element(Note)]}.

make_entry_element(Note) ->
    #xmlel{name = <<"entry">>,
           attrs = [{<<"xmlns">>, ?NS_ATOM}],
           children = make_note_fields(Note)}.

make_note_fields(Note) ->
    SimpleFields = [make_simple_element(Name, maps:get(Name, Note)) ||
                    Name <- [content, title, id]],
    TimeFields = [make_time_element(Name, maps:get(Name, Note)) ||
                  Name <- [published, updated]],
    MediaFields = get_media_fields(maps:get(media, Note)),

    SimpleFields ++ TimeFields ++ MediaFields.

make_simple_element(Name, Value) ->
    #xmlel{name = atom_to_binary(Name, utf8),
           children = [#xmlcdata{content = Value}]}.

make_time_element(Name, Value) ->
    TimeBin = wocky_db:timestamp_to_string(Value),
    make_simple_element(Name, TimeBin).

get_media_fields(MediaBin) ->
    case exml:parse(MediaBin) of
        {ok, Element} -> [Element];
        {error, _} -> []
    end.

notify_subscriber(From, To, Message) ->
    ejabberd_router:route(From, To, Message).

retract_note(From = #jid{lserver = LServer}, BotID, NoteID) ->
    wocky_db_bot:delete_note(LServer, BotID, NoteID),
    Message = notification_message(BotID, [retract_item(NoteID)]),
    notify_subscribers(From, BotID, Message).

retract_item(NoteID) ->
    #xmlel{name = <<"retract">>,
           attrs = [{<<"id">>, NoteID}]}.

notification_message(BotID, ItemEls) ->
    #xmlel{name = <<"message">>,
           children = [notification_event(BotID, ItemEls)]}.

notification_event(BotID, ItemEls) ->
    #xmlel{name = <<"event">>,
           attrs = [{<<"xmlns">>, ?NS_BOT}],
           children = [notification_items(BotID, ItemEls)]}.

notification_items(BotID, ItemEls) ->
    #xmlel{name = <<"items">>,
           attrs = [{<<"node">>, BotID}],
           children = ItemEls}.

get_notes(LServer, BotID, RSM) ->
    Notes = wocky_db_bot:get_notes(LServer, BotID),
    {ok, rsm_util:filter_with_rsm(Notes, RSM)}.

make_results(Notes, RSMOut) ->
    #xmlel{name = <<"query">>,
           attrs = [{<<"xmlns">>, ?NS_BOT}],
           children =
           make_notes(Notes) ++
           jlib:rsm_encode(RSMOut)
          }.

make_notes(Notes) ->
    [make_note_item(Note) || Note <- Notes].
