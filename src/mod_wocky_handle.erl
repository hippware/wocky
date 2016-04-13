%%% @copyright 2016+ Hippware, Inc.
%%% @doc Module to process handle to JID lookups
-module(mod_wocky_handle).

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
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_HANDLE,
                                  ?MODULE, handle_iq, IQDisc).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_HANDLE).


%%%===================================================================
%%% IQ handler callback
%%%===================================================================

handle_iq(_From, _To, #iq{type = get, sub_el = #xmlel{children = Els}} = IQ) ->
    Users = lookup_handles(handles_from_xml(Els)),
    iq_result(IQ, users_to_xml(Users));
handle_iq(_From, _To, #iq{type = set} = IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_NOT_ALLOWED]}.

handles_from_xml(Els) ->
    [handle_from_xml(El) || #xmlel{name = <<"item">>} = El <- Els].

handle_from_xml(El) ->
    attr_value(xml:get_tag_attr(<<"id">>, El)).

attr_value({value, Handle}) -> Handle;
attr_value(Value) -> Value.

lookup_handles(Handles) ->
    lists:foldr(fun (false, Acc) -> Acc;
                    (Handle, UserData) ->
                        Result = lookup_handle(Handle),
                        [{Handle, Result} | UserData]
                end, [], Handles).

lookup_handle(Handle) ->
    case wocky_db:select_one(shared, handle_to_user, user,
                             #{handle => Handle}) of
        not_found -> not_found;
        null -> not_found;
        User ->
            Columns = [user, server, first_name, last_name],
            wocky_db:select_row(shared, user, Columns, #{user => User})
    end.

users_to_xml(Users) ->
    [user_to_xml(User) || User <- Users].

user_to_xml({Handle, UserData}) ->
    #xmlel{name = <<"item">>,
           attrs = [{<<"id">>, Handle} | xml_user_attrs(UserData)]}.

xml_user_attrs(#{user := User, server := Server,
                 first_name := FirstName, last_name := LastName}) ->
    [{<<"jid">>, jid:to_binary({User, Server, <<>>})},
     {<<"firstname">>, safe_string(FirstName)},
     {<<"lastname">>, safe_string(LastName)}];
xml_user_attrs(not_found) ->
    [{<<"error">>, <<"item-not-found">>}].

safe_string(null) -> <<>>;
safe_string(Str) when is_binary(Str) -> Str.

iq_result(IQ, Content) ->
    IQ#iq{type = result,
          sub_el = [#xmlel{name = <<"results">>,
                           attrs = [{<<"xmlns">>, ?NS_HANDLE}],
                           children = Content}]}.
