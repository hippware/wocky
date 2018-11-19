%%% @copyright 2016+ Hippware, Inc.
%%% @doc Module to handle message push notifications
-module(mod_wocky_notifications).

-compile({parse_transform, cut}).

-include("wocky.hrl").
-include("wocky_roster.hrl").

%% gen_mod behaviour
-behaviour(gen_mod).
-export([start/2, stop/1]).

%% Hook callbacks
-export([remove_user_hook/3]).

%% IQ handler
-export([handle_iq/3]).


%%%===================================================================
%%% gen_mod implementation
%%%===================================================================

-spec start(ejabberd:server(), list()) -> any().
start(Host, _Opts) ->
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_NOTIFICATIONS,
                                  ?MODULE, handle_iq, parallel),
    wocky_util:add_hooks(hooks(), Host, ?MODULE, 100).

-spec stop(ejabberd:server()) -> ok.
stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_NOTIFICATIONS),
    wocky_util:delete_hooks(hooks(), Host, ?MODULE, 100).

hooks() -> [{remove_user, remove_user_hook}].


%%%===================================================================
%%% IQ handler
%%%===================================================================

-spec handle_iq(ejabberd:jid(), ejabberd:jid(), iq()) -> iq().
handle_iq(From, _To, IQ = #iq{type = set, sub_el = ReqEl}) ->
    {ok, State} = handle_request(From, ReqEl),
    make_response(IQ, State);
handle_iq(_From, _To, IQ) ->
    make_error_response(IQ, ?ERRT_NOT_ALLOWED(?MYLANG, <<"not allowed">>)).

handle_request(J, #xmlel{name = <<"enable">>, attrs = Attrs}) ->
    {value, DeviceId} = xml:get_attr(<<"device">>, Attrs),
    User = ?wocky_user:get_by_jid(J),
    ok = ?wocky_push:enable(User, J#jid.lresource, DeviceId),
    {ok, <<"enabled">>};
handle_request(J, #xmlel{name = <<"disable">>}) ->
    User = ?wocky_user:get_by_jid(J),
    ok = ?wocky_push:disable(User, J#jid.lresource),
    {ok, <<"disabled">>}.

make_error_response(IQ, ErrStanza) ->
    ok = lager:warning("Error on notification IQ request: ~p", [ErrStanza]),
    IQ#iq{type = error, sub_el = ErrStanza}.

make_response(IQ, State) ->
    IQ#iq{type = result,
          sub_el = #xmlel{name = State,
                          attrs = [{<<"xmlns">>, ?NS_NOTIFICATIONS}]}}.

%% remove_user -------------------------------------------------------
remove_user_hook(Acc, UserID, _Server) ->
    User = ?wocky_user:get_user(UserID),
    ?wocky_push:purge(User),
    Acc.
