%%% @doc Module replacing cyrsasl_plain's SASL PLAIN handling to add
%%% our own custom Digits auth system
%%% See https://github.com/hippware/tr-wiki/wiki/User-registration-XMPP-protocol

%%% Adopted from cyrsasl_plain.erl:
%%%----------------------------------------------------------------------
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : PLAIN SASL mechanism
%%% Created :  8 Mar 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_wocky_sasl_plain).
-xep([{xep, 78}, {version, "2.5"}]).

-include("wocky.hrl").
-include("wocky_reg.hrl").

-behaviour(gen_mod).
-behaviour(cyrsasl).

%% gen_mod handlers
-export([start/2, stop/1]).

%% cyrsasl handlers
-export([mech_new/2, mech_step/2]).


start(_Host, Opts) ->
    Providers = proplists:get_value(auth_providers, Opts),
    {atomic, _} = ejabberd_config:add_local_option(wocky_sasl_auth_providers,
                                                   Providers),
    BypassPrefixes = get_auth_bypass_prefixes(Opts),
    {atomic, _} = ejabberd_config:add_local_option(wocky_sasl_bypass_prefixes,
                                                   BypassPrefixes),

    % This *replaces* cyrsasl_plain's registration, since modules are
    % loaded/started after cyrsasl has registered its built-in modules.
    cyrsasl:register_mechanism(<<"PLAIN">>, ?MODULE, plain),
    ok.

stop(_Host) ->
    ok.

-spec mech_new(Host :: ejabberd:server(), Creds :: mongoose_credentials:t()) ->
    {ok, tuple()}.
mech_new(Host, Creds) ->
    cyrsasl_plain:mech_new(Host, Creds).

-spec mech_step(Creds :: mongoose_credentials:t(), ClientIn :: binary()) ->
    {ok, mongoose_credentials:t()} | {error, binary()}.
mech_step(Creds, ClientIn) ->
    %% There is a regression in ejabberd_auth that causes authentication
    %% failures to return internal-error instead of not-authorized. We are
    %% working around that regression here.
    case cyrsasl_plain:parse(ClientIn) of
        [_, <<"register">>, <<"$J$", JSON/binary>>] ->
            do_registration(JSON);
        [AuthzId, User, Password] ->
            Request = mongoose_credentials:extend(Creds,
                                                  [{username, User},
                                                   {password, Password},
                                                   {authzid, AuthzId}]),
            case ejabberd_auth:authorize(Request) of
                {ok, Result} ->
                    {ok, Result};
                {error, not_authorized} ->
                    {error, <<"not-authorized">>, User};
                {error, {no_auth_modules, _}} ->
                    {error, <<"not-authorized">>, User};
                {error, R} ->
                    ok = lager:debug("authorize error: ~p", [R]),
                    {error, <<"internal-error">>}
            end;
        _ ->
            {error, <<"bad-protocol">>}
    end.

enable_digits_bypass() ->
    ?confex:get(wocky_xmpp, enable_digits_bypass).

get_auth_bypass_prefixes(Opts) ->
  case enable_digits_bypass() of
    true  -> proplists:get_value(auth_bypass_prefixes, Opts, []);
    false -> []
  end.

do_registration(JSON) ->
    case wocky_reg:register_user(JSON) of
        {ok, RegResult} ->
            make_register_response(RegResult);
        {error, {Response, Text}} ->
            {error, {iolist_to_binary(Response), iolist_to_binary(Text)}}
    end.

make_register_response(#reg_result{user = User,
                                   server = Server,
                                   provider = Provider,
                                   is_new = IsNew,
                                   token = Token,
                                   token_expiry = TokenExpiry,
                                   external_id = ExternalID}) ->
   Handle = case ?wocky_repo:get(?wocky_user, User) of
                nil -> <<>>;
                #{handle := nil} -> <<>>;
                #{handle := H} -> H
            end,
   JSONFields = [{user, User},
                 {server, Server},
                 {handle, Handle},
                 {provider, Provider},
                 {is_new, IsNew},
                 {external_id, ExternalID} |
                 maybe_token_fields(Token, TokenExpiry)],
   JSON = mochijson2:encode({struct, JSONFields}),
   {error, {<<"redirect">>, JSON}}.

maybe_token_fields(undefined, _) -> [];
maybe_token_fields(Token, TokenExpiry) ->
   [{token, Token},
    {token_expiry, TokenExpiry}].
