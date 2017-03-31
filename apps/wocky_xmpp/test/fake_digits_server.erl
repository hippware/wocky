%%% @copyright 2016+ Hippware, Inc.
%%% @doc Simple server to emulate the interface of the Twitter Digits
%%% authroization server.
-module(fake_digits_server).

-behaviour(cowboy_http_handler).

-export([start/2, stop/0, url/0, init/3, handle/2, terminate/3]).

-include("wocky_db_seed.hrl").

%% Start the server. `AllowAuth' specifies whether requests to this server
%% should succeed or fail.
start(AllowAuth, PhoneNumber) ->
    cowboy:start_http(fake_digits_listener, 1,
                      [{port, port()}],
                      [{env, [{dispatch,
                               make_dispatch(#{allow_auth => AllowAuth,
                                               phone_number => PhoneNumber})
                      }]}]).

stop() ->
    ok = cowboy:stop_listener(fake_digits_listener).

make_dispatch(Env) ->
    cowboy_router:compile([{'_', [{'_', ?MODULE, Env}]}]).

init({_TransportName, http}, Req, AllowAuth) ->
    {ok, Req, AllowAuth}.

handle(Req, Env = #{allow_auth := AllowAuth, phone_number := PhoneNumber}) ->
    {ok, Req2} = case AllowAuth of
        true -> cowboy_req:reply(200, headers(), body(PhoneNumber), Req);
        false -> cowboy_req:reply(401, Req)
    end,
    {ok, Req2, Env}.

terminate(_Reason, _Req, _State) -> ok.

url() -> "http://localhost:" ++ integer_to_list(port()).
port() -> 9999.

headers() -> [{"content-type", "application/json"}].
body(PhoneNumber) ->
    JSON = [{phone_number, PhoneNumber},
            {access_token, {struct,
                            %% Obviously these aren't valid, but we don't
                            %% check them so just jam anything in for now
                            [{token, <<"asflkjasdlfjsadflkjs">>},
                             {secret, <<"sdlkfjslf">>}]}},
            {id_str, ?EXTERNAL_ID},
            {verification_type, <<"sms">>},
            {id, binary_to_integer(?EXTERNAL_ID)},
            {created_at, <<"Wed Nov 18 09:33:48 +0000 2015">>}],
    mochijson2:encode({struct, JSON}).
