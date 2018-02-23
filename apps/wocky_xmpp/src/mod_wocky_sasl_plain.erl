%%% @doc Module replacing cyrsasl_plain's SASL PLAIN handling to add
%%% our own custom OAuth 2 auth system
%%% See https://github.com/hippware/tr-wiki/wiki/User-registration-XMPP-protocol

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
mech_step(_, <<0:8, "register", 0:8, "$J$", JSON/binary>>) ->
  case wocky_reg:register_user(JSON) of
      {ok, RegResult} ->
          make_register_response(RegResult);
      {error, {Response, Text}} ->
          {error, {iolist_to_binary(Response), iolist_to_binary(Text)}}
  end;
mech_step(Creds, ClientIn) ->
  cyrsasl_plain:mech_step(Creds, ClientIn).

enable_auth_bypass() ->
    ?confex:get_env(wocky_xmpp, enable_auth_bypass).

get_auth_bypass_prefixes(Opts) ->
  case enable_auth_bypass() of
    true  -> proplists:get_value(auth_bypass_prefixes, Opts, []);
    false -> []
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
