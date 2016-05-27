%%% @copyright 2016+ Hippware, Inc.
%%% @doc Wocky interface to Algolia for full text search of users
-module(wocky_db_user_idx).

-behaviour(gen_server).

%% API
-export([start_link/0,
         user_created/2,
         user_updated/2,
         user_removed/1]).

-ignore_xref([{start_link, 0}, {user_created, 2}]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {enabled = false :: boolean(),
                client :: term(),
                index :: term()}).
-type state() :: #state{}.


%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts the server
-spec start_link() -> {ok, pid()} | ignore | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Called after a user is created to update the index.
-spec user_created(binary(), map()) -> ok.
user_created(UserID, Data) ->
    gen_server:cast(?SERVER, {user_created, UserID, Data}).

%% @doc Called after a user is updated to update the index.
-spec user_updated(binary(), map()) -> ok.
user_updated(UserID, Data) ->
    gen_server:cast(?SERVER, {user_updated, UserID, Data}).

%% @doc Called after a user is removed to update the index.
-spec user_removed(binary()) -> ok.
user_removed(UserID) ->
    gen_server:cast(?SERVER, {user_removed, UserID}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec init([]) -> {ok, state()}.
init([]) ->
    {ok, ID} = application:get_env(wocky, algolia_app_id),
    {ok, Key} = application:get_env(wocky, algolia_app_key),
    {ok, IdxName} = application:get_env(wocky, algolia_index_name),
    {ok, IndexingEnvs} = application:get_env(wocky, indexing_enabled_envs),
    {ok, CurrentEnv} = application:get_env(wocky, wocky_env),

    Enabled = lists:member(CurrentEnv, IndexingEnvs),
    Client = algolia:make_client(ID, Key),
    Index = algolia:init_index(Client, IdxName),

    ok = lager:info("Indexing enabled: ~p", [Enabled]),

    {ok, #state{enabled = Enabled,
                client = Client,
                index = Index}}.

%% @private
%% @doc Handling call messages
-spec handle_call(any(), {pid(), any()}, state()) -> {reply, ok, state()}.
handle_call(_Request, _From, State) ->
    {reply, {error, bad_call}, State}.

%% @private
%% @doc Handling cast messages
-spec handle_cast({atom(), binary()} | {atom(), binary(), map()} | any(),
                  state()) -> {noreply, state()}.
handle_cast(_Msg, #state{enabled = false} = State) ->
    %% do nothing
    {noreply, State};
handle_cast({user_created, UserID, Data}, #state{index = Index} = State) ->
    Object = map_to_object(UserID, Data),
    ok = lager:debug("Updating user index with new object ~p", [Object]),
    {ok, _} = algolia_index:add_object(Index, Object),
    {noreply, State};
handle_cast({user_updated, UserID, Data}, #state{index = Index} = State) ->
    Object = map_to_object(UserID, Data),
    ok = lager:debug("Updating user index with object ~p", [Object]),
    {ok, _} = algolia_index:update_object(Index, Object),
    {noreply, State};
handle_cast({user_removed, UserID}, #state{index = Index} = State) ->
    ok = lager:debug("Removing user ~s from index", [UserID]),
    {ok, _} = algolia_index:delete_object(Index, UserID),
    {noreply, State};
handle_cast(Msg, State) ->
    ok = lager:warning("Unhandled cast: ~p", [Msg]),
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec terminate(any(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

map_to_object(UserID, MapData) ->
    maps:fold(fun (K, V, Acc) -> Acc#{atom_to_binary(K, utf8) => V} end,
              #{<<"objectID">> => UserID},
              maps:with([handle, last_name, first_name, email], MapData)).
