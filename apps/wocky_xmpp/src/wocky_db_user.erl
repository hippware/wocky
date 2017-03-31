%%% @copyright 2015+ Hippware, Inc.
%%% @doc Wocky database interface for "users"
%%%
%%% == Terminology ==
%%% <dl>
%%% <dt>JID</dt>
%%% <dd>
%%%   Short for "Jabber IDentifier" and defined in RFC 6122, the JID is the
%%%   canonical identifier for a specific user within the Jabber network.
%%%
%%%   Briefly, the JID is composed of three parts: the "localpart" that
%%%   identifies the user, the "domainpart" that identifies the Jabber network
%%%   and the "resourcepart" that identifies the current connection to the
%%%   network.
%%%
%%% </dd>
%%% <dt>User/LUser</dt>
%%% <dd>
%%%   The "localpart" of the JID.
%%%
%%%   For Wocky it is a timeuuid generated when the user is created and
%%%   formatted as a canonical UUID string. This is the canonical user ID within
%%%   Wocky but is not meant for display to the end user.
%%%
%%%   The "L" prefix indicates that the string has been normalized according to
%%%   RFC 6122. Variables without the "L" prefix are assumed to not be
%%%   normalized and must be processed before use.
%%%
%%% </dd>
%%% <dt>Server/LServer</dt>
%%% <dd>
%%%   The "domainpart" of the JID.
%%%
%%%   For Wocky this is the virtual host that the user connects to. This is not
%%%   the physical machine that is handling the user's connection, but a domain
%%%   representing a single cluster of application instances.
%%%
%%%   The "L" prefix indicates that the string has been normalized according to
%%%   RFC 6122. Variables without the "L" prefix are assumed to not be
%%%   normalized and must be processed before use.
%%%
%%% </dd>
%%% <dt>Handle</dt>
%%% <dd>
%%%   This is the name that is displayed to the user and is chosen by the user
%%%   when they create their account.
%%%
%%%   It must be globally unique.
%%%
%%% </dd>
%%% </dl>
%%%
%%% @reference See <a href="http://xmpp.org/rfcs/rfc6122.html">RFC 6122</a>
%%% for more information on JIDs.
%%%
%%% @reference See <a href="https://tools.ietf.org/html/rfc4122">RFC 4122</a>
%%% for the definition of UUIDs.
%%%
%%% @reference
%%% <a href="https://en.wikipedia.org/wiki/Universally_unique_identifier">
%%% This Wikipedia article</a> provides a good overview of UUIDs.
%%%
-module(wocky_db_user).

-include("wocky.hrl").

-type handle()       :: binary().
-type phone_number() :: binary().
-type password()     :: binary().
-type token()        :: binary().
-export_type([handle/0, phone_number/0, password/0, token/0]).

%% API
-export([
         set_location/6
        ]).


-compile({parse_transform, do}).


%%%===================================================================
%%% API
%%%===================================================================


%% @doc Updates the user's location.
%%
%% `LUser': the "localpart" of the user's JID.
%%
%% `LServer': the "domainpart" of the user's JID.
%%
%% `LResource': the "resourcepart" of the user's JID.
%%
%% `Lat': the latitude of the user's location in degrees North
%%
%% `Lon': the longditude of the user's location in degrees East
%%
%% `Accuracy': the accuracy of the user's location in meters
%%
-spec set_location(ejabberd:luser(), ejabberd:lserver(), ejabberd:lresource(),
                   number(), number(), number()) -> ok.
set_location(LUser, LServer, LResource, Lat, Lon, Accuracy) ->
    wocky_db:insert(LServer, location, #{user =>     LUser,
                                         server =>   LServer,
                                         resource => LResource,
                                         time =>     now,
                                         lat =>      Lat,
                                         lon =>      Lon,
                                         accuracy => Accuracy}).
