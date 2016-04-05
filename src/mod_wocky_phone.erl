%%% @copyright 2016+ Hippware, Inc.
%%% @doc Module to handle token related IQs
-module(mod_wocky_phone).

-include_lib("ejabberd/include/jlib.hrl").
-include("wocky.hrl").

%% gen_mod behaviour
-behaviour(gen_mod).
-export([start/2, stop/1]).

%% IQ handler callback
-export([handle_iq/3]).

-ifdef(TEST).
-export([lookup_reductions/2, save_reductions/3]).
-endif.

-define(DEFAULT_REDUCTIONS, 5000).
-define(REDUCTION_TTL, 86400). % 24 hours

%% calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})
-define(EPOCH_SECONDS, 62167219200).


%%%===================================================================
%%% gen_mod implementation
%%%===================================================================

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_PHONE,
                                  ?MODULE, handle_iq, IQDisc).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_PHONE).


%%%===================================================================
%%% IQ handler callback
%%%===================================================================

handle_iq(From, _To, #iq{type = get, sub_el = #xmlel{children = Els}} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = From,
    case wocky_db:is_valid_id(LUser) of
        true ->
            handle_iq_get(LUser, LServer, Els, IQ);

        false ->
            IQ#iq{type = error, sub_el = [?ERR_JID_MALFORMED]}
    end;
handle_iq(_From, _To, #iq{type = set} = IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_NOT_ALLOWED]}.

handle_iq_get(User, Server, Els, IQ) ->
    Reductions = lookup_reductions(Server, User),
    {Users, Remaining} = lookup_numbers(numbers_from_xml(Els), Reductions),
    ok = save_reductions(Server, User, Remaining),
    iq_result(IQ, users_to_xml(Users)).

lookup_reductions(Server, User) ->
    Criteria = #{user => User, server => Server, date => get_date()},
    case wocky_db:select_one(Server, phone_lookup_count, count, Criteria) of
        not_found -> ?DEFAULT_REDUCTIONS;
        null -> ?DEFAULT_REDUCTIONS;
        Reductions -> Reductions
    end.

save_reductions(Server, User, Reductions) ->
    ok = wocky_db:insert(Server, phone_lookup_count,
                         #{user => User,
                           server => Server,
                           date => get_date(),
                           count => Reductions,
                           '[ttl]' => ?REDUCTION_TTL}).

get_date() ->
    {Date, _Time} = calendar:universal_time(),
    Seconds = calendar:datetime_to_gregorian_seconds({Date, {0, 0, 0}}),
    wocky_db:seconds_to_timestamp(Seconds - ?EPOCH_SECONDS).

numbers_from_xml(Els) ->
    [number_from_xml(El) || #xmlel{name = <<"item">>} = El <- Els].

number_from_xml(El) ->
    attr_value(xml:get_tag_attr(<<"id">>, El)).

attr_value({value, Number}) -> Number;
attr_value(Value) -> Value.

lookup_numbers(Numbers, StartingReductions) ->
    lists:foldr(fun (false, Acc) -> Acc;
                    (Number, {UserData, Reductions}) ->
                        {Result, Remaining} = lookup_number(Number, Reductions),
                        {[{Number, Result} | UserData], Remaining}
                end, {[], StartingReductions}, Numbers).

lookup_number(_Number, 0) ->
    {not_acceptable, 0};
lookup_number(Number, Reductions) ->
    {lookup_number(Number), Reductions - 1}.

lookup_number(Number) ->
    case wocky_db:select_one(shared, phone_number_to_user, user,
                             #{phone_number => maybe_add_plus(Number)}) of
        not_found -> not_found;
        null -> not_found;
        User ->
            Columns = [user, server, handle, first_name, last_name],
            wocky_db:select_row(shared, user, Columns, #{user => User})
    end.

maybe_add_plus(<<"+", _/binary>> = String) -> String;
maybe_add_plus(String) -> <<"+", String/binary>>.

users_to_xml(Users) ->
    [user_to_xml(User) || User <- Users].

user_to_xml({Number, UserData}) ->
    #xmlel{name = <<"item">>,
           attrs = [{<<"id">>, Number} | xml_user_attrs(UserData)]}.

xml_user_attrs(#{user := User, server := Server, handle := Handle,
                 first_name := FirstName, last_name := LastName}) ->
    [{<<"jid">>, jid:to_binary({User, Server, <<>>})},
     {<<"handle">>, safe_string(Handle)},
     {<<"firstname">>, safe_string(FirstName)},
     {<<"lastname">>, safe_string(LastName)}];
xml_user_attrs(not_acceptable) ->
    [{<<"error">>, <<"not-acceptable">>}];
xml_user_attrs(not_found) ->
    [{<<"error">>, <<"item-not-found">>}].

safe_string(null) -> <<>>;
safe_string(Str) when is_binary(Str) -> Str.

iq_result(IQ, Content) ->
    IQ#iq{type = result,
          sub_el = [#xmlel{name = <<"results">>,
                           attrs = [{<<"xmlns">>, ?NS_PHONE}],
                           children = Content}]}.
