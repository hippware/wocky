%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module implementing geolocation (XEP-0080)
%%% See http://xmpp.org/extensions/xep-0080.html
%%%
-module(mod_wocky_geoloc).

-behaviour(gen_mod).
-behaviour(pep_elem_handler).

-compile({parse_transform, do}).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").
-include("wocky.hrl").
-include("wocky_roster.hrl").

%% gen_mod handlers
-export([start/2, stop/1]).

%% mod_wocky_pep handler
-export([handle_pep/2]).

%%%===================================================================
%%% gen_mod handlers
%%%===================================================================

start(_Host, _Opts) ->
    mod_wocky_pep:register_handler(?NS_GEOLOC, whitelist, ?MODULE).

stop(_Host) ->
    mod_wocky_pep:unregister_handler(?NS_GEOLOC, whitelist, ?MODULE).


%%%===================================================================
%%% mod_wocky_pep callback
%%%===================================================================

-spec handle_pep(jid(), exml:element()) -> exml:element().
handle_pep(_From, Item = #xmlel{name = <<"geoloc">>, children = []}) ->
    % Special case for end-of-location-data message (see
    % XEP-0080 s4.1). This _should_ be rebroadcast in spite of lacking
    % lat and lon fields.
    Item;
handle_pep(From, Item = #xmlel{name = <<"geoloc">>}) ->
    case handle_geoloc(Item) of
        {ok, {_Lat, _Lon, _Accuracy} = Loc} ->
            'Elixir.Wocky.Location':user_location_changed(From, Loc),
            Item;
        {error, Reason} ->
            ok = lager:info("Error processing geoloc IQ: ~p", [Reason]),
            drop
    end;
handle_pep(_From, Item) ->
    Item.

handle_geoloc(Item) ->
    do([error_m ||
        Lat <- get_item(Item, <<"lat">>),
        Lon <- get_item(Item, <<"lon">>),
        Accuracy <- get_item(Item, <<"accuracy">>, 0),
        {ok, {Lat, Lon, Accuracy}}
       ]).

get_item(Item, Name, Default) ->
    case get_item(Item, Name) of
        {error, {not_found, _}} -> {ok, Default};
        {ok, Val} -> {ok, Val}
    end.

get_item(Item, Name) ->
    case xml:get_path_s(Item, [{elem, Name}, cdata]) of
        <<>> -> {error, {not_found, Name}};
        Data -> wocky_util:safe_bin_to_float(Data)
    end.
