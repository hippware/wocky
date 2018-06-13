%%% @copyright 2016+ Hippware, Inc.
%%% @doc Module to handle phone number and handle lookups
-module(mod_wocky_lookup).

-include("wocky.hrl").

%% gen_mod behaviour
-behaviour(gen_mod).
-export([start/2, stop/1]).

%% IQ handler callback
-export([handle_phone_iq/3, handle_handle_iq/3]).


%%%===================================================================
%%% gen_mod implementation
%%%===================================================================

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_PHONE,
                                  ?MODULE, handle_phone_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_HANDLE,
                                  ?MODULE, handle_handle_iq, IQDisc).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_PHONE),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_HANDLE).


%%%===================================================================
%%% Phone lookup IQ handler callback
%%%===================================================================

handle_phone_iq(From, _To, #iq{type = get} = IQ) ->
    #iq{sub_el = #xmlel{children = Els}} = IQ,
    #jid{luser = LUser, lserver = LServer} = From,
    handle_phone_iq_get(LUser, LServer, Els, IQ);
handle_phone_iq(_From, _To, #iq{type = set} = IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_NOT_ALLOWED]}.

handle_phone_iq_get(_User, _Server, Els, IQ) ->
    Users = lookup_numbers(items_from_xml(Els)),
    iq_result(IQ, users_to_xml(Users)).

lookup_numbers(Numbers) ->
    lists:foldr(fun (false, Acc) -> Acc;
                    (Number, Acc) ->
                        Result = lookup_number(Number),
                        [{Number, Result} | Acc]
                end, [], Numbers).

lookup_number(Number) ->
    ?wocky_repo:get_by(?wocky_user, [{phone_number, maybe_add_plus(Number)}]).

maybe_add_plus(<<"+", _/binary>> = String) -> String;
maybe_add_plus(String) -> <<"+", String/binary>>.


%%%===================================================================
%%% Handle lookup IQ handler callback
%%%===================================================================

handle_handle_iq(_From, _To, #iq{type = get} = IQ) ->
    #iq{sub_el = #xmlel{children = Els}} = IQ,
    Users = lookup_handles(items_from_xml(Els)),
    iq_result(IQ, users_to_xml(Users));
handle_handle_iq(_From, _To, #iq{type = set} = IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_NOT_ALLOWED]}.

lookup_handles(Handles) ->
    lists:foldr(fun (false, Acc) -> Acc;
                    (Handle, UserData) ->
                        Result = lookup_handle(Handle),
                        [{Handle, Result} | UserData]
                end, [], Handles).

lookup_handle(Handle) ->
    ?wocky_repo:get_by(?wocky_user, [{handle, Handle}]).


%%%===================================================================
%%% Helper functions
%%%===================================================================

items_from_xml(Els) ->
    [item_from_xml(El) || #xmlel{name = <<"item">>} = El <- Els].

item_from_xml(El) ->
    attr_value(xml:get_tag_attr(<<"id">>, El)).

attr_value({value, Value}) -> Value;
attr_value(Value) -> Value.

users_to_xml(Users) ->
    [user_to_xml(User) || User <- lists:flatten(Users)].

user_to_xml({Number, UserData}) ->
    #xmlel{name = <<"item">>,
           attrs = [{<<"id">>, Number} | xml_user_attrs(UserData)]}.

xml_user_attrs(#{id := ID} = User) ->
    [{<<"jid">>, jid:to_binary({ID, ?wocky:host(), <<>>})},
     {<<"handle">>, get_safe(handle, User)},
     {<<"first_name">>, get_safe(first_name, User)},
     {<<"last_name">>, get_safe(last_name, User)},
     {<<"avatar">>, get_safe(avatar, User)}];
xml_user_attrs(not_acceptable) ->
    [{<<"error">>, <<"not-acceptable">>}];
xml_user_attrs(nil) ->
    [{<<"error">>, <<"item-not-found">>}].

get_safe(Key, Map) ->
    safe_string(maps:get(Key, Map, nil)).

safe_string(nil) -> <<>>;
safe_string(Str) when is_binary(Str) -> Str.

iq_result(IQ, Content) ->
    IQ#iq{type = result,
          sub_el = [#xmlel{name = <<"results">>,
                           attrs = [{<<"xmlns">>, ?NS_PHONE}],
                           children = Content}]}.
