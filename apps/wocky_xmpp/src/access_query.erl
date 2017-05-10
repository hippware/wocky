%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module providing access query functionality
%%% See https://github.com/hippware/tr-wiki/wiki/Access-Control-Query
%%%
-module(access_query).

-compile({parse_transform, cut}).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").
-include("wocky.hrl").

-export([run/3]).

-define(IQ_TIMEOUT, 2000).
-define(QUERY_TIMEOUT, (?IQ_TIMEOUT + 1000)).
% Arbitrary but bigger than I can see us needing at the moment:
-define(MAX_REDIRECTS, 5).

-spec run(ejabberd:jid(), ejabberd:jid(), mod_wocky_access:op()) ->
    mod_wocky_access:access_result().
run(#jid{lserver = LServer, resource = LResource}, Actor, Op) ->
    do_query(LServer, LResource, Actor, Op, []).

do_query(LServer, LResource, Actor, Op, Redirects)
  when length(Redirects) > ?MAX_REDIRECTS ->
    ok = lager:info("Maximum redirects reached for ~p/~p - ~p/~p",
                    [LServer, LResource, Actor, Op]),
    deny;
do_query(LServer, LRescource, Actor, Op, Redirects) ->
    IQ = make_access_query(LRescource, Actor, Op),
    Waiter = self(),
    ejabberd_local:route_iq(aq_jid(), jid:make(<<>>, LServer, <<>>),
                            IQ, handle_reply(Waiter, _), ?IQ_TIMEOUT),
    Result = await_result(),
    handle_result(Result, LServer, LRescource, Actor, Op, Redirects).

await_result() ->
    receive
        {response, Response} -> Response
    after ?QUERY_TIMEOUT -> deny
    end.

handle_result(deny, _, _, _, _, _) -> deny;
handle_result(allow, _, _, _, _, _) -> allow;
handle_result({redirect, Target}, LServer, LResource, Actor, Op, Redirects) ->
    case lists:member(Target, Redirects) of
        true ->
            ok = lager:info("Redirect loop for ~p/~p - ~p/~p",
                            [LServer, LResource, Actor, Op]),
            deny;
        false ->
            #jid{lserver = NewLServer, lresource = NewLResource} =
            jid:from_binary(Target),
            do_query(NewLServer, NewLResource, Actor, Op, [Target | Redirects])
    end.

aq_jid() ->
    jid:make(<<>>, wocky_xmpp_app:server(), <<>>).

make_access_query(Resource, Actor, Op) ->
    #iq{type = get,
        sub_el = #xmlel{name = <<"query">>,
                        attrs = [{<<"xmlns">>, ?NS_ACCESS},
                                 {<<"node">>, Resource},
                                 {<<"actor">>, jid:to_binary(Actor)},
                                 {<<"op">>, atom_to_binary(Op, utf8)}]}}.

handle_reply(Pid, timeout) ->
    Pid ! {response, deny};
handle_reply(Pid, #iq{type = result, sub_el = SubEls}) ->
    Result = find_result(SubEls),
    Pid ! {response, Result}.

find_result([#xmlel{name = <<"allow">>} | _]) -> allow;
find_result([#xmlel{name = <<"deny">>} | _]) -> deny;
find_result([#xmlel{name = <<"redirect">>,
                    children = [#xmlcdata{content = Target}]} | _]) ->
    {redirect, Target};
find_result([]) -> deny;
find_result([_ | Rest]) -> find_result(Rest).
