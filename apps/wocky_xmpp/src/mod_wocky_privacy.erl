%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module implementing wocky-specific privacy filtering
%%%
-module(mod_wocky_privacy).

-include("wocky.hrl").

-behaviour(gen_mod).

%% gen_mod handlers
-export([start/2, stop/1]).

%% Privacy filter callback
-export([check_packet/6]).

-define(CHECK_PACKET_PRIORITY, 90).


%%%===================================================================
%%% gen_mod handlers
%%%===================================================================

start(Host, _Opts) ->
    ejabberd_hooks:add(privacy_check_packet, Host,
                       ?MODULE, check_packet, ?CHECK_PACKET_PRIORITY).

stop(Host) ->
    ejabberd_hooks:delete(privacy_check_packet, Host,
                          ?MODULE, check_packet, ?CHECK_PACKET_PRIORITY).

%%%===================================================================
%%% Privacy check callback
%%%===================================================================

-spec check_packet(mongoose_acc:t(), ejabberd:luser(), ejabberd:lserver(),
                   mod_privacy:userlist(),
                   {ejabberd:jid(), ejabberd:jid(), binary(), binary()},
                   in | out) -> mongoose_acc:t().
check_packet(#{result := Result} = Acc, _User, _Server, _UserList,
             {From, To, Name, _Type}, Dir) ->
    do_check_packet(Result, From, To, Name, Dir, Acc).

% Allow messages sent by bare servers, possibly with resources.
% We trust our own servers, so this should be fine.
% This is used by messages from bots and chat rooms.
do_check_packet(deny, #jid{luser = <<>>}, _To, _Name, in, Acc) ->
    mongoose_acc:put(result, allow, Acc);

% Allow outgoing presences to bots (overriding the default denial of
% presence_out). These are used for temporary subscriptions.
do_check_packet(deny, _From,
                #jid{luser = <<>>, lresource = <<"bot/", _/binary>>},
                <<"presence">>, out, Acc) ->
    mongoose_acc:put(result, allow, Acc);

do_check_packet(_, _, _, _, _, Acc) -> Acc.
