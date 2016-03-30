%%% @copyright 2016+ Hippware, Inc.
%%% @doc TROS request tracker for Francus interface
-module(tros_req_tracker).

-export([
         start/0,
         stop/0,
         add/1,
         get_delete/1,
         expire/1
        ]).

-include("mod_tros_francus.hrl").

start() ->
    tros_requests = ets:new(tros_requests, [named_table, public,
                                            {keypos, #tros_request.request}]).

stop() ->
    ets:delete(tros_requests).

add(Req) ->
    {ok, TRef} = timer:apply_after(timeout(), ?MODULE, expire,
                                   [Req#tros_request.request]),
    ets:insert(tros_requests, Req#tros_request{tref = TRef}).

expire(Request) ->
    ets:delete(tros_requests, Request).

get_delete(RequestKey) ->
    case ets:lookup(tros_requests, RequestKey) of
        [AuthReq = #tros_request{tref = TRef}] ->
            %% Timer may already fired - ignore return:
            _ = timer:cancel(TRef),
            ets:delete_object(tros_requests, AuthReq),
            AuthReq;
        [] ->
            false
    end.

timeout() -> ejabberd_config:get_local_option(tros_auth_validity).
