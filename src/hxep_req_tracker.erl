-module(hxep_req_tracker).

-export([
         start/0,
         stop/0,
         add/1,
         get_delete/1,
         expire/1
        ]).

-include("mod_hxep_francus.hrl").

-define(DEFAULT_TIMEOUT, timer:seconds(60)).

start() ->
    hxep_requests = ets:new(hxep_requests, [named_table, public,
                                            {keypos, #hxep_request.request}]).

stop() ->
    ets:delete(hxep_requests).

add(Req) ->
    {ok, TRef} = timer:apply_after(timeout(), ?MODULE, expire,
                                   [Req#hxep_request.request]),
    ets:insert(hxep_requests, Req#hxep_request{tref = TRef}).

expire(Request) ->
    ets:delete(hxep_requests, Request).

get_delete(RequestKey) ->
    case ets:lookup(hxep_requests, RequestKey) of
        [AuthReq = #hxep_request{tref = TRef}] ->
            %% Timer may already fired - ignore return:
            _ = timer:cancel(TRef),
            ets:delete_object(hxep_requests, AuthReq),
            AuthReq;
        [] ->
            false
    end.

timeout() -> ?DEFAULT_TIMEOUT.
