%%% @copyright 2016+ Hippware, Inc.
%%% @doc Wocky user registration server
%%%
%%% This module implements the Wocky DB reset REST endpoint for testing
-module(wocky_rest_db).

%% Webmachine callbacks
-export([init/1,
         resource_exists/2,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         malformed_request/2,
         forbidden/2,
         post_is_create/2,
         create_path/2,
         from_json/2,
         to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(state, { user   :: binary(),
                 server :: binary(),
                 token  :: binary() }).

%%%===================================================================
%%% Webmachine Callbacks
%%%===================================================================

init(Opts) ->
    Server = proplists:get_value(server, Opts),
    {ok, #state{ server = Server }}.

allowed_methods(RD, Ctx) ->
    {['POST'], RD, Ctx}.

resource_exists(RD, Ctx) ->
    Info = wrq:path_info(RD),
    case Info of
        [{operation, "reset"}] -> {true, RD, Ctx};
        [{operation, "delete"}] -> {true, RD, Ctx};
        _ -> {false, RD, Ctx}
    end.

content_types_accepted(RD, Ctx) ->
    {[{"application/json", from_json}], RD, Ctx}.

content_types_provided(RD, Ctx) ->
    {[{"application/json", to_json}], RD, Ctx}.

malformed_request(RD, Ctx) ->
    Body = wrq:req_body(RD),
    try mochijson2:decode(Body) of
        {struct, Elements} ->
            is_malformed(Elements, RD, Ctx)
    catch
        Class:Reason ->
            ST = lager:pr_stacktrace(erlang:get_stacktrace(), {Class, Reason}),
            Msg = io_lib:format("Invalid JSON. ~p ~s", [Body, ST]),
            ok = lager:error(Msg),
            {true, set_resp_body(400, Msg, RD), Ctx}
    end.

forbidden(RD, Ctx = #state{user = User, server = Server, token = Token}) ->
    case verify_session(User, Server, Token) of
        true ->
            {false, RD, Ctx};

        false ->
            RD2 = set_resp_body(403, "SessionID could not be verified", RD),
            {true, RD2, Ctx}
    end.

post_is_create(RD, Ctx) -> {true, RD, Ctx}.

create_path(RD, Ctx) -> {"", RD, Ctx}.

from_json(RD, Ctx = #state{user = User, server = Server}) ->
    case wrq:path_info(RD) of
        [{operation, "reset"}] ->
            wocky_db_seed:bootstrap(shared, Server),
            wocky_db_seed:bootstrap(Server, Server),
            {true, RD, Ctx};

        [{operation, "delete"}] ->
            case ejabberd_auth:remove_user(User, Server) of
                ok ->
                    {true, RD, Ctx};

                Error ->
                    Msg = io_lib:format("Error removing user: ~p", [Error]),
                    {true, set_resp_body(500, Msg, RD), Ctx}
            end
    end.

% This function is required to keep webmachine happy (since it must be
% specified in content_types_provided, which in turn is required to avoid
% errors if the client inclused an 'Accept' header) but is not actually
% called because we're using post_is_create.
to_json(RD, Ctx) ->
    {wrq:resp_body(RD), RD, Ctx}.

%%%===================================================================
%%% Helpers
%%%===================================================================

is_malformed(Elements, RD, Ctx) ->
    Fields = wocky_rest:map_keys_to_atoms(
               maps:from_list(Elements)),
    case verify_fields(Fields) of
        true ->
            #{uuid := User, sessionID := Token} = Fields,
            {false, RD, Ctx#state{user = User, token = Token}};

        false ->
            RD2 = set_resp_body(400, "Missing field(s)", RD),
            {true, RD2, Ctx}
    end.

verify_fields(#{sessionID := _, uuid := _}) -> true;
verify_fields(_) -> false.

verify_session(User, Server, Token) ->
    wocky_db_user:check_token(User, Server, Token).

set_resp_body(Code, Error, RD) ->
    JSON = mochijson2:encode(
             {struct, [{code, Code}, {error, iolist_to_binary(Error)}]}),
    wrq:set_resp_body(JSON, RD).
