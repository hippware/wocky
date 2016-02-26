-module(fake_digits_server).

-export([start/1, stop/0, url/0, init/3, handle/2, terminate/3]).

-include("wocky_db_seed.hrl").

start(AllowAuth) ->
    Dispatch = cowboy_router:compile([{'_', [{'_', ?MODULE, AllowAuth}]}]),
    cowboy:start_https(fake_digits_listener, 1,
                       [
                        {port, port()} |
                        wocky_util:ssl_opts()
                       ],
                       [{env, [{dispatch, Dispatch}]}]).

stop() ->
    ok = cowboy:stop_listener(fake_digits_listener).

init({_TransportName, http}, Req, AllowAuth) ->
    {ok, Req, AllowAuth}.

handle(Req, AllowAuth) ->
    Req2 = case AllowAuth of
        true -> cowboy_req:reply(200, headers(), body(), Req);
        false -> cowboy_req:reply(401, Req)
    end,
    {ok, Req2, AllowAuth}.

terminate(_Reason, _Req, _State) -> ok.

url() -> "https://localhost:" ++ integer_to_list(port()).
port() -> 9999.

headers() -> [{"content-type", "application/json"}].
body() ->
    JSON = [{phone_number, ?PHONE_NUMBER},
            {access_token, {struct,
                            %% Obviously these aren't valid, but we don't
                            %% check them so just jam anything in for now
                            [{token, <<"asflkjasdlfjsadflkjs">>},
                             {secret, <<"sdlkfjslf">>}]}},
            {id_str, <<"701990807448920064">>},
            {verification_type, <<"sms">>},
            {id, 701990807448920064},
            {created_at, <<"Wed Nov 18 09:33:48 +0000 2015">>}],
    mochijson2:encode({struct, JSON}).
