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
         to_json/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").

-record(state, {
          server         :: binary(),
          fields         :: map()
         }).

%%%===================================================================
%%% Webmachine Callbacks
%%%===================================================================

init(Opts) ->
    Server = proplists:get_value(server, Opts),
    {ok, #state{
            server = Server
           }}.

allowed_methods(RD, Ctx) ->
    {['POST'], RD, Ctx}.

resource_exists(RD, Ctx) ->
    Info = wrq:path_info(RD),
    case Info of
        [{operation, "reset"}] -> {true, RD, Ctx};
        _ -> {false, RD, Ctx}
    end.

content_types_accepted(RD, Ctx) ->
    {[{"application/json", from_json}], RD, Ctx}.

content_types_provided(RD, Ctx) ->
    {[{"application/json", to_json}], RD, Ctx}.

malformed_request(RD, Ctx) ->
    try mochijson2:decode(wrq:req_body(RD)) of
        {struct, Elements} ->
            is_malformed(Elements, RD, Ctx)
    catch
        error:_ ->
            RD2 = set_resp_body(400, "Invalid JSON", RD),
            {true, RD2, Ctx}
    end.

forbidden(RD, Ctx = #state{fields = Fields,
                           server = Server}) ->
    case verify_session(Fields, Server) of
        true ->
            {false, RD, Ctx};
        false ->
            RD2 = set_resp_body(403, "SessionID could not be verified", RD),
            {true, RD2, Ctx}
    end.

post_is_create(RD, Ctx) -> {true, RD, Ctx}.

create_path(RD, Ctx) -> {"", RD, Ctx}.

from_json(RD, Ctx = #state{server = Server}) ->
    %% Currently 'reset' is the only operation (and already checked for
    %% in 'resource_exists/2', so just do that if we get this far.
    wocky_db_seed:bootstrap(shared),
    wocky_db_seed:bootstrap(Server),
    {true, RD, Ctx}.

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
            {false, RD, Ctx#state{fields = Fields}};
        false ->
            RD2 = set_resp_body(400, "Missing field(s)", RD),
            {true, RD2, Ctx}
    end.

verify_fields(#{sessionID := _,
                uuid      := _,
                resource  := _
               }) -> true;
verify_fields(_) -> false.

verify_session(#{uuid := UUID, sessionID := SessionID, resource := Resource},
               Server) ->
    wocky_db_user:check_token(UUID, Server, Resource, SessionID).

set_resp_body(Code, Error, RD) ->
    JSON = mochijson2:encode({struct, [{code, Code},
                                       {error, list_to_binary(Error)}]}),
    wrq:set_resp_body(JSON, RD).
