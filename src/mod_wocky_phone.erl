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

handle_iq(_From, _To, #iq{type = get, sub_el = #xmlel{children = Els}} = IQ) ->
    Users = lookup_numbers(numbers_from_xml(Els)),
    iq_result(IQ, users_to_xml(Users)).

numbers_from_xml(Els) ->
    [number_from_xml(El) || #xmlel{name = <<"item">>} = El <- Els].

number_from_xml(El) ->
    attr_value(xml:get_tag_attr(<<"id">>, El)).

attr_value({value, Number}) -> Number;
attr_value(Value) -> Value.

lookup_numbers(Numbers) ->
    [{Number, lookup_number(Number)} || Number <- Numbers, Number =/= false].

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
xml_user_attrs(not_found) ->
    [{<<"error">>, <<"item-not-found">>}].

safe_string(null) -> <<>>;
safe_string(Str) when is_binary(Str) -> Str.

iq_result(IQ, Content) ->
    IQ#iq{type = result,
          sub_el = [#xmlel{name = <<"results">>,
                           attrs = [{<<"xmlns">>, ?NS_PHONE}],
                           children = Content}]}.
