%%% @copyright 2016+ Hippware, Inc.
%%% @doc Server to monitor and notify expiry of bot follow-me
-module(wocky_bot_expiry_mon).

-compile({parse_transform, cut}).

-include("wocky.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0,
         follow_started/2,
         follow_stopped/1,
         set_warning_time/1
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(WARNING_TIME, timer:minutes(10)).

-record(state, {bots = #{} :: map(), %% #{BotJID => [timer_ref()]}
                warning_time = ?WARNING_TIME :: non_neg_integer()
               }).
-type state() :: #state{}.


%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts the server
-spec start_link() -> {ok, pid()} | ignore | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Adds a bot being followed. Expiry is in seconds.
-spec follow_started(jid(), non_neg_integer()) -> ok.
follow_started(JID, ExpiryS) ->
    gen_server:cast(?SERVER, {follow_started, jid:to_binary(JID), ExpiryS}).

%% @doc Removes a bot being followed and cancels timers.
-spec follow_stopped(jid()) -> ok.
follow_stopped(JID) ->
    gen_server:cast(?SERVER, {follow_stopped, jid:to_binary(JID)}).

%% @doc Override the default warning time (for testing purposes)
-spec set_warning_time(non_neg_integer()) -> ok.
set_warning_time(Time) ->
    gen_server:cast(?SERVER, {set_warning_time, Time}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(_) ->
    {ok, #state{}}.

%% @private
%% @doc Handling call messages
-spec handle_call(any(), {pid(), any()}, state()) ->
    {reply, {error, bad_call}, state()}.
handle_call(_Request, _From, State) ->
    {reply, {error, bad_call}, State}.

%% @private
%% @doc Handling cast messages
-spec handle_cast({atom(), binary()} | {atom(), binary(), map()} | any(),
                  state()) -> {noreply, state()}.

handle_cast({follow_started, JID, ExpiryS},
            State = #state{bots = Bots, warning_time = WarningTime}) ->
    NewBots = start_timers(JID, ExpiryS, WarningTime, Bots),
    {noreply, State#state{bots = NewBots}};

handle_cast({follow_stopped, JID}, State = #state{bots = Bots}) ->
    NewBots = stop_existing_timers(JID, Bots),
    {noreply, State#state{bots = NewBots}};

handle_cast({set_warning_time, Time}, State) ->
    {noreply, State#state{warning_time = timer:seconds(Time)}};

handle_cast(Msg, State) ->
    ok = lager:warning("Unhandled cast: ~p", [Msg]),
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info({timeout, Ref, JID}, State = #state{bots = Bots}) ->
    NewBots = remove_timer_ref(JID, Ref, Bots),
    send_expiry_warning(JID),
    {noreply, State#state{bots = NewBots}};
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

start_timers(JID, ExpiryS, WarningTime, Bots) ->
    CleanBots = stop_existing_timers(JID, Bots),
    Now = ?wocky_timestamp:now(),
    ExpiryMS = timer:seconds(ExpiryS) - Now,
    WarningMS = ExpiryMS - WarningTime,
    References = maybe_set_timers(JID, [ExpiryMS, WarningMS]),
    maybe_store_references(JID, References, CleanBots).

stop_existing_timers(JID, Bots) ->
    case maps:get(JID, Bots, undefined) of
        undefined ->
            Bots;
        Timers ->
            stop_timers(Timers),
            maps:remove(JID, Bots)
    end.

maybe_set_timers(JID, Periods) ->
    lists:foldl(maybe_set_timer(JID, _, _), [], Periods).

maybe_set_timer(JID, Period, Acc) when Period > 0 ->
    %% Period comes from an unbounded expiry value from the client, so it can be
    %% big enough to make start_timer choke. If it's that big, just ignore it
    %% and leave the timer unset.
    try
        [erlang:start_timer(Period, self(), JID) | Acc]
    catch
        _:_ -> Acc
    end;
maybe_set_timer(_, _, Acc) -> Acc.

maybe_store_references(_JID, [], Bots) -> Bots;
maybe_store_references(JID, References, Bots) -> Bots#{JID => References}.

stop_timers(References) ->
    lists:foreach(erlang:cancel_timer(_, []), References).

send_expiry_warning(JIDBin) when is_binary(JIDBin) ->
    BotID = ?wocky_bot:get_id_from_jid(jid:from_binary(JIDBin)),
    case ?wocky_repo:get(?wocky_bot, BotID) of
        nil -> ok;
        Bot -> send_expiry_warning(Bot)
    end;
send_expiry_warning(#{user_id := OwnerID, follow_me_expiry := Expiry} = Bot) ->
    OwnerJID = ?wocky_user:to_jid(?wocky_repo:get(?wocky_user, OwnerID)),
    BotJID = ?wocky_bot:to_jid(Bot),
    ExpiryStr = integer_to_binary(Expiry div 1000),
    Stanza =
        wocky_bot_util:follow_stanza(Bot, {<<"follow expire">>, ExpiryStr}),
    ejabberd_router:route(BotJID, OwnerJID, Stanza).

remove_timer_ref(JID, Ref, Bots) ->
    case maps:get(JID, Bots, undefined) of
        undefined ->
            Bots;
        Refs ->
            NewRefs = Refs -- [Ref],
            maybe_store_refs(JID, NewRefs, maps:remove(JID, Bots))
    end.

maybe_store_refs(_JID, [], Bots) -> Bots;
maybe_store_refs(JID, Refs, Bots) -> Bots#{JID => Refs}.
