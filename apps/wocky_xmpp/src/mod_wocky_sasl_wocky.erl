%%% @doc Module implementing a custom SASL mechanism 'X-WOCKY'.
%%% Authentication is handled via a third-party OAuth 2 provider and the
%%% payload is a custom JWT.

-module(mod_wocky_sasl_wocky).

-include("wocky.hrl").

-behaviour(gen_mod).
-behaviour(cyrsasl).

%% gen_mod handlers
-export([start/2, stop/1]).

%% cyrsasl handlers
-export([mech_new/2, mech_step/2]).

-record(state, {creds}).

start(_Host, _Opts) ->
    cyrsasl:register_mechanism(<<"X-WOCKY">>, ?MODULE, plain),
    ok.

stop(_Host) ->
    ok.

-spec mech_new(Host :: ejabberd:server(), Creds :: mongoose_credentials:t()) ->
    {ok, tuple()}.
mech_new(_Host, Creds) ->
    {ok, #state{creds = Creds}}.

-spec mech_step(Creds :: mongoose_credentials:t(), ClientIn :: binary()) ->
    {ok, mongoose_credentials:t()} | {error, binary()}.
mech_step(#state{creds = Creds}, SerializedToken) ->
    %% SerializedToken is a token decoded from CDATA <auth/> body sent by client
    Server = wocky_xmpp_app:server(),
    case ?wocky_account:authenticate(client_jwt, Server, SerializedToken) of
        {ok, {#{id := UserID}, _} ->
            {ok, mongoose_credentials:extend(Creds, [{username, UserID}])};
        {error, {Username, _}} ->
            {error, <<"not-authorized">>, Username};
        {error, _Reason} ->
            {error, <<"not-authorized">>}
    end.
