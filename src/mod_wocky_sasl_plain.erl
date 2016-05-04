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

-include("wocky_reg.hrl").

-behaviour(gen_mod).
-behaviour(cyrsasl).

%% gen_mod handlers
-export([start/2, stop/1]).

%% cyrsasl handlers
-export([mech_new/4, mech_step/2]).

-record(state, {check_password}).

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

-spec mech_new(Host :: ejabberd:server(),
               GetPassword :: cyrsasl:get_password_fun(),
               CheckPassword :: cyrsasl:check_password_fun(),
               CheckPasswordDigest :: cyrsasl:check_pass_digest_fun()
               ) -> {ok, tuple()}.
mech_new(_Host, _GetPassword, CheckPassword, _CheckPasswordDigest) ->
    {ok, #state{check_password = CheckPassword}}.

-spec mech_step(State :: tuple(),
                ClientIn :: binary()
               ) -> {ok, proplists:proplist()} | {error, binary()}.
mech_step(State, ClientIn) ->
    case prepare(ClientIn) of
        [_AuthzId, <<"register">>, <<"$J$", JSON/binary>>] ->
            do_registration(JSON);
        [AuthzId, User, Password] ->
            case (State#state.check_password)(User,
                                              Password
                                             ) of
                {true, AuthModule} ->
                    {ok, [{username, User}, {authzid, AuthzId},
                          {auth_module, AuthModule}]};
                _ ->
                    {error, <<"not-authorized">>, User}
            end;
        _ ->
            {error, <<"bad-protocol">>}
    end.

-spec prepare(binary()) -> 'error' | [binary(),...].
prepare(ClientIn) ->
    case parse(ClientIn) of
        [<<>>, UserMaybeDomain, Password] ->
            case parse_domain(UserMaybeDomain) of
                %% <NUL>login@domain<NUL>pwd
                [User, _Domain] ->
                    [UserMaybeDomain,
                     User,
                     Password];
                %% <NUL>login<NUL>pwd
                [User] ->
                    [<<>>, User, Password]
            end;
        %% login@domain<NUL>login<NUL>pwd
        [AuthzId, User, Password] ->
            [AuthzId, User, Password];
        _ ->
            error
    end.


-spec parse(binary()) -> [binary(),...].
parse(S) ->
    parse1(S, <<>>, []).

-spec parse1(binary(),binary(),[binary()]) -> [binary(),...].
parse1(<<0, Cs/binary>>, S, T) ->
    parse1(Cs, <<>>, [binary_reverse(S)| T]);
parse1(<<C, Cs/binary>>, S, T) ->
    parse1(Cs, <<C, S/binary>>, T);
parse1(<<>>, S, T) ->
    lists:reverse([binary_reverse(S)| T]).


-spec parse_domain(binary()) -> [binary(),...].
parse_domain(S) ->
    parse_domain1(S, <<>>, []).

-spec parse_domain1(binary(),binary(),[binary()]) -> [binary(),...].
parse_domain1(<<$@, Cs/binary>>, S, T) ->
    parse_domain1(Cs, <<>>, [binary_reverse(S) | T]);
parse_domain1(<<C, Cs/binary>>, S, T) ->
    parse_domain1(Cs, <<C, S/binary>>, T);
parse_domain1(<<>>, S, T) ->
    lists:reverse([binary_reverse(S) | T]).


-spec binary_reverse(binary()) -> binary().
binary_reverse(<<>>) ->
    <<>>;
binary_reverse(<<H,T/binary>>) ->
    <<(binary_reverse(T))/binary,H>>.

get_auth_bypass_prefixes(Opts) ->
  case wocky_app:is_testing() of
    true  -> proplists:get_value(auth_bypass_prefixes, Opts, []);
    false -> []
  end.

do_registration(JSON) ->
    case wocky_reg:register_user(JSON) of
        {ok, RegResult} ->
            make_register_response(RegResult);
        {error, {Response, Text}} ->
            {false, {list_to_binary(Response), list_to_binary(Text)}}
    end.

make_register_response(#reg_result{user = User,
                                   server = Server,
                                   provider = Provider,
                                   is_new = IsNew,
                                   token = Token,
                                   token_expiry = TokenExpiry,
                                   external_id = ExternalID}) ->
   Handle = wocky_db_user:get_handle(User, Server),
   JSONFields = [{user, User},
                 {server, Server},
                 {handle, Handle},
                 {provider, Provider},
                 {is_new, IsNew},
                 {token, Token},
                 {token_expiry, TokenExpiry},
                 {external_id, ExternalID}],
   JSON = mochijson2:encode(JSONFields),
   {false, {<<"redirect">>, JSON}}.


