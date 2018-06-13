%%%----------------------------------------------------------------------
%%% File    : mod_wocky_roster.erl
%%% Purpose : Roster management
%%%
%%% ejabberd, Copyright (C) 2002-2013   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------
-module(mod_wocky_roster).

-compile({parse_transform, do}).

-include("wocky.hrl").
-include("wocky_roster.hrl").
-include_lib("mongooseim/include/mod_roster.hrl").

%% gen_mod behaviour
-behaviour(gen_mod).
-export([start/2, stop/1]).

%% IQ handler callback
-export([process_iq/3]).

%% Hook callbacks
-export([roster_get_hook/2,
         roster_in_subscription_hook/6,
         roster_out_subscription_hook/5,
         roster_get_subscription_lists_hook/3,
         roster_get_jid_info_hook/4,
         roster_get_versioning_feature_hook/2,
         filter_local_packet_hook/1,
         roster_modified_hook/4
        ]).

-import(wocky_roster, [to_wocky_roster/1, to_wocky_roster/3]).

-define(NULL_VERSION, <<"0">>).

%%%===================================================================
%%% gen_mod implementation
%%%===================================================================

start(Host, Opts) ->
    wocky_util:add_hooks(hooks(), Host, ?MODULE, 50),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_ROSTER,
                                  ?MODULE, process_iq, IQDisc).

stop(Host) ->
    wocky_util:delete_hooks(hooks(), Host, ?MODULE, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_ROSTER).

hooks() ->
    [{roster_get,                    roster_get_hook},
     {roster_in_subscription,        roster_in_subscription_hook},
     {roster_out_subscription,       roster_out_subscription_hook},
     {roster_get_subscription_lists, roster_get_subscription_lists_hook},
     {roster_get_jid_info,           roster_get_jid_info_hook},
     {roster_get_versioning_feature, roster_get_versioning_feature_hook},
     {filter_local_packet,           filter_local_packet_hook},
     {roster_modified,               roster_modified_hook}
    ].

%%%===================================================================
%%% IQ handler callback
%%%===================================================================

process_iq(#jid{lserver = LServer} = From, To, #iq{sub_el = SubEl} = IQ) ->
    case lists:member(LServer, ?MYHOSTS) of
        true ->
            process_local_iq(From, To, IQ);
        _ ->
            IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]}
    end.

process_local_iq(From, To, #iq{type = get} = IQ) ->
    process_iq_get(From, To, IQ);
process_local_iq(From, To, #iq{type = set} = IQ) ->
    process_iq_set(From, To, IQ).

%% Load roster from DB only if neccesary.
%% It is neccesary if
%%     - roster versioning is not used by the client OR
%%     - the roster version from client don't match current version.
process_iq_get(From, To, #iq{sub_el = SubEl} = IQ) ->
    try
        AttrVer = xml:get_tag_attr(<<"ver">>, SubEl),
        {ItemsToSend, VersionToSend} =
            get_user_roster_based_on_version(AttrVer, From, To),
        IQ#iq{type = result,
              sub_el = create_sub_el(ItemsToSend, VersionToSend)}
    catch
        Class:Reason ->
            ok = lager:error("Error retrieving roster for user '~ts': ~ts",
                             [jid:to_binary(From),
                              lager:pr_stacktrace(erlang:get_stacktrace(),
                                                  {Class, Reason})]),
            IQ#iq{type = error,
                  sub_el = [SubEl, ?ERR_INTERNAL_SERVER_ERROR]}
    end.

get_user_roster_based_on_version(false, From, To) ->
    get_user_roster_based_on_version({value, ?NULL_VERSION}, From, To);
get_user_roster_based_on_version({value, RequestedVersion}, From, To) ->
    #jid{luser = LUser, lserver = LServer} = From,
    case ?wocky_roster:version(LUser) of
        ?NULL_VERSION ->
            {[], ?NULL_VERSION};

        RequestedVersion ->
            {[], RequestedVersion};

        NewVersion ->
            US = {LUser, LServer},
            ToServer = To#jid.lserver,
            {lists:map(fun (Item) -> item_to_xml(Item) end,
                       ejabberd_hooks:run_fold(roster_get, ToServer, [], [US])),
             NewVersion}
    end.

create_sub_el(Items, Version) ->
    [#xmlel{name = <<"query">>,
            attrs = [{<<"xmlns">>, ?NS_ROSTER} |
                     maybe_version(Items, Version)],
            children = Items}].

% If there are no items, there shouldn't be a version attribute
maybe_version([], _) -> [];
maybe_version(_, Version) -> [{<<"ver">>, Version}].

process_iq_set(#jid{lserver = LServer} = From, To, #iq{sub_el = SubEl} = IQ) ->
    #xmlel{children = Els} = SubEl,
    ejabberd_hooks:run(roster_set, LServer, [From, To, SubEl]),
    Result = lists:foldl(
               fun(El, Result) ->
                       process_item_set(From, To, El, Result)
               end, ok, Els),
    case Result of
        ok -> IQ#iq{type = result, sub_el = []};
        {error, Error} -> wocky_util:make_error_iq_response(IQ, Error)
    end.

process_item_set(From, To, #xmlel{attrs = Attrs} = El, Result) ->
    JID1 = jid:from_binary(xml:get_attr_s(<<"jid">>, Attrs)),
    do_process_item_set(JID1, From, To, El, Result);
process_item_set(_From, _To, _, Result) -> Result.

do_process_item_set(error, _, _, _, Result) -> Result;
do_process_item_set(JID1, From, To, El, Result) ->
    #jid{luser = LUser} = From,
    #jid{luser = ContactUser} = JID1,

    OldItem = to_wocky_roster(
                LUser, ContactUser,
                ?wocky_roster:get(LUser, ContactUser)),

    User = ?wocky_repo:get(?wocky_user, LUser),
    Contact = ?wocky_repo:get(?wocky_user, ContactUser),
    do_process_item_set_1(Contact, User, To, OldItem, El, Result).

do_process_item_set_1(nil, _, _, _, _, _) ->
    {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Error: Invalid contact ID">>)};

do_process_item_set_1(#{id := ContactID} = Contact,
                      #{id := UserID} = User, ToJID, OldItem,
                      #xmlel{attrs = Attrs, children = Els}, Result) ->
    Item1 = process_item_attrs(OldItem, Attrs),
    Item2 = #wocky_roster{groups = NewGroups}
        = process_item_els(Item1#wocky_roster{groups = []}, Els),

    % For an unblocking action, reinitialise the items and clear the
    % blocked/blocked_by groups on both
    is_unblock(User, Contact, NewGroups)
    andalso
    ?wocky_block:unblock(User, Contact),

    % For a new block, immediately mark both the roster items as blocked
    % and send the unsubscription to the blocked user
    FromJID = ?wocky_user:to_jid(User),
    case is_new_block(User, Contact, NewGroups) of
        true ->
            ?wocky_block:block(User, Contact),

            BlockNotification = to_wocky_roster(
                                  ?wocky_roster:get(ContactID, UserID)),
            push_item(ContactID, ?wocky:host(), FromJID,
                      OldItem, BlockNotification),
            Result;
        false ->
            do_process_item_set_2(Item2, OldItem, FromJID,
                                  ToJID, ContactID, Result)
    end.

do_process_item_set_2(NewItem, OldItem,
                      From =#jid{user = User, luser = LUser, lserver = LServer},
                      To, ContactUser, Result) ->
    ChangeResult = case NewItem#wocky_roster.subscription of
        remove ->
            CR = ?wocky_roster:delete(LUser, ContactUser),
            send_unsubscribing_presence(From, OldItem),
            CR;
        _ ->
            ?wocky_roster:put(wocky_roster:to_map(NewItem))
    end,

    case ChangeResult of
        {error, Error} ->
            ErrStr = ?wocky_errors:render_errors(?wocky_errors:to_map(Error)),
            {error, ?ERRT_BAD_REQUEST(
                       ?MYLANG, <<"Error(s): ", ErrStr/binary>>)};
        _ ->
            Item = ejabberd_hooks:run_fold(roster_process_item, LServer,
                                           NewItem, [LServer]),
            push_item(User, LServer, To, OldItem, Item),
            Result
    end.

is_new_block(User, Contact, NewGroups) ->
    (not ?wocky_block:'blocked?'(User, Contact)) andalso has_block(NewGroups).

is_unblock(User, Contact, NewGroups) ->
    ?wocky_block:'blocked?'(User, Contact) andalso (not has_block(NewGroups)).

has_block(Groups) ->
    lists:member(?wocky_roster:blocked_group(), Groups).

process_item_attrs(Item, [{<<"jid">>, Val} | Attrs]) ->
    case jid:from_binary(Val) of
        error ->
            process_item_attrs(Item, Attrs);
        JID1 ->
            JID = {JID1#jid.luser, JID1#jid.lserver, JID1#jid.lresource},
            process_item_attrs(Item#wocky_roster{contact_jid = JID}, Attrs)
    end;
process_item_attrs(Item, [{<<"handle">>, Val} | Attrs]) ->
    process_item_attrs(Item#wocky_roster{contact_handle = Val}, Attrs);
process_item_attrs(Item, [{<<"first_name">>, Val} | Attrs]) ->
    process_item_attrs(Item#wocky_roster{first_name = Val}, Attrs);
process_item_attrs(Item, [{<<"last_name">>, Val} | Attrs]) ->
    process_item_attrs(Item#wocky_roster{last_name = Val}, Attrs);
process_item_attrs(Item, [{<<"name">>, Val} | Attrs]) ->
    process_item_attrs(Item#wocky_roster{name = Val}, Attrs);
process_item_attrs(Item, [{<<"avatar">>, Val} | Attrs]) ->
    process_item_attrs(Item#wocky_roster{avatar = Val}, Attrs);
process_item_attrs(Item, [{<<"subscription">>, <<"remove">>} | Attrs]) ->
    process_item_attrs(Item#wocky_roster{subscription = remove}, Attrs);
process_item_attrs(Item, [_ | Attrs]) ->
    process_item_attrs(Item, Attrs);
process_item_attrs(Item, []) ->
    Item.

process_item_els(Item, [#xmlel{name = <<"group">>} = El | Els]) ->
    Groups = [xml:get_cdata(El#xmlel.children) | Item#wocky_roster.groups],
    process_item_els(Item#wocky_roster{groups = Groups}, Els);
process_item_els(Item, [#xmlel{} = El | Els]) ->
    #xmlel{name = Name, attrs = Attrs, children = SEls} = El,
    case xml:get_attr_s(<<"xmlns">>, Attrs) of
        <<"">> -> process_item_els(Item, Els);
        _ ->
            XEls = [#xmlel{name = Name, attrs = Attrs, children = SEls}
                    | Item#wocky_roster.xs],
            process_item_els(Item#wocky_roster{xs = XEls}, Els)
    end;
process_item_els(Item, [{xmlcdata, _} | Els]) ->
    process_item_els(Item, Els);
process_item_els(Item, []) -> Item.


%%%===================================================================
%%% Hook callbacks
%%%===================================================================

%% roster_get --------------------------------------------------------

roster_get_hook(Acc, {LUser, _LServer}) ->
    Items = to_wocky_roster(?wocky_roster:get(LUser)),
    lists:filter(fun (#wocky_roster{subscription = none, ask = in}) ->
                         false;
                     (_) ->
                         true
                 end, Items ++ Acc).

%% roster_in_subscription, roster_out_subscription -------------------

roster_in_subscription_hook(Acc, User, Server, JID, Type, Reason) ->
    Res = process_subscription(in, User, Server, JID, Type, Reason),
    mongoose_acc:put(result, Res, Acc).

roster_out_subscription_hook(Acc, User, Server, JID, Type) ->
    Res = process_subscription(out, User, Server, JID, Type, <<"">>),
    mongoose_acc:put(result, Res, Acc).

process_subscription(Direction, User, Server, JID1, Type, _Reason) ->
    LUser = jid:nodeprep(User),
    #jid{luser = ContactUser} = JID1,

    Item = to_wocky_roster(LUser, jid:to_lower(JID1),
                           ?wocky_roster:get(LUser, ContactUser)),
    #wocky_roster{subscription = Subscription, ask = Ask} = Item,

    StateChange = state_change(Direction, Subscription, Ask, Type),
    Action = process_state_change(StateChange, Item),
    Push = do_roster_action(Action),

    ToJID = jid:make(User, Server, <<"">>),
    AutoReply = get_auto_reply(Direction, Subscription, Ask, Type),
    do_auto_reply(ToJID, JID1, Item, AutoReply),

    case Push of
        {push, OldItem, NewItem} ->
            push_item(User, Server, ToJID, OldItem, NewItem),
            true;
        none ->
            false
    end.

state_change(in, S, A, T) -> in_state_change(S, A, T);
state_change(out, S, A, T) -> out_state_change(S, A, T).

%% X_state_change(CurrentSubscription, CurrentPending, RequestType) -> Action

% These cases should never be hit in wocky - auto subscription success means
% we'll never have a pending value of `in` or `both`
in_state_change(_,    in,   _)            -> erlang:error(invalid_roster_state);
in_state_change(_,    both, _)            -> erlang:error(invalid_roster_state);

in_state_change(none, none, subscribe)    -> {add_new, {from, none}};
in_state_change(none, none, subscribed)   -> none;
in_state_change(none, none, unsubscribe)  -> none;
in_state_change(none, none, unsubscribed) -> none;

in_state_change(none, out,  subscribe)    -> {add_new, {from, none}};
in_state_change(none, out,  subscribed)   -> {to, none};
in_state_change(none, out,  unsubscribe)  -> none;
in_state_change(none, out,  unsubscribed) -> {none, none};

in_state_change(to,   none, subscribe)    -> {both, none};
in_state_change(to,   none, subscribed)   -> none;
in_state_change(to,   none, unsubscribe)  -> none;
in_state_change(to,   none, unsubscribed) -> {none, none};

in_state_change(from, none, subscribe)    -> none;
in_state_change(from, none, subscribed)   -> {both, none};
in_state_change(from, none, unsubscribe)  -> {none, none};
in_state_change(from, none, unsubscribed) -> none;

in_state_change(from, out,  subscribe)    -> none;
in_state_change(from, out,  subscribed)   -> {both, none};
in_state_change(from, out,  unsubscribe)  -> {none, out};
in_state_change(from, out,  unsubscribed) -> {from, none};

in_state_change(both, none, subscribe)    -> none;
in_state_change(both, none, subscribed)   -> none;
in_state_change(both, none, unsubscribe)  -> {to, none};
in_state_change(both, none, unsubscribed) -> {from, none}.

out_state_change(none, none, subscribe)    -> {none, out};
out_state_change(none, none, subscribed)   -> none;
out_state_change(none, none, unsubscribe)  -> none;
out_state_change(none, none, unsubscribed) -> none;

out_state_change(none, out,  subscribe)    -> {none, out};
out_state_change(none, out,  subscribed)   -> none;
out_state_change(none, out,  unsubscribe)  -> {none, none};
out_state_change(none, out,  unsubscribed) -> none;

out_state_change(to,   none, subscribe)    -> none;
out_state_change(to,   none, subscribed)   -> {both, none};
out_state_change(to,   none, unsubscribe)  -> {none, none};
out_state_change(to,   none, unsubscribed) -> none;

out_state_change(from, none, subscribe)    -> {from, out};
out_state_change(from, none, subscribed)   -> none;
out_state_change(from, none, unsubscribe)  -> none;
out_state_change(from, none, unsubscribed) -> {none, none};

out_state_change(from, out,  subscribe)    -> none;
out_state_change(from, out,  subscribed)   -> none;
out_state_change(from, out,  unsubscribe)  -> {from, none};
out_state_change(from, out,  unsubscribed) -> {none, out};

out_state_change(both, none, subscribe)    -> none;
out_state_change(both, none, subscribed)   -> none;
out_state_change(both, none, unsubscribe)  -> {from, none};
out_state_change(both, none, unsubscribed) -> {to, none}.

process_state_change(none, _) ->
    none;

process_state_change({add_new, {NewSubscription, NewPending}}, Item) ->
    NewItem = Item#wocky_roster{groups = [<<"__new__">>]},
    process_state_change({NewSubscription, NewPending}, NewItem);

process_state_change({NewSubscription, Pending}, Item) ->
    NewItem = Item#wocky_roster{
                subscription = NewSubscription,
                ask = Pending
               },
    {insert, Item, NewItem}.

do_roster_action(none) ->
    none;
do_roster_action({insert, OldItem, NewItem}) ->
    {ok, _} = ?wocky_roster:put(wocky_roster:to_map(NewItem)),
    {push, OldItem, NewItem}.

get_auto_reply(out, _, _, _) -> none;
get_auto_reply(in, S, A, T) -> in_auto_reply(S, A, T).

% in_auto_reply(CurrentSubscription, CurrentPending, Request) -> Action
% Subscription requests always succeed automatically in wocky:
in_auto_reply(_,    _,    subscribe)   -> subscribed;
in_auto_reply(from, none, unsubscribe) -> unsubscribed;
in_auto_reply(from, out,  unsubscribe) -> unsubscribed;
in_auto_reply(both, none, unsubscribe) -> unsubscribed;
in_auto_reply(_,    _,    _)           -> none.

do_auto_reply(_, _, _, none) -> ok;
do_auto_reply(ToJID, JID1, _Item, unsubscribed) ->
    Attrs = [{<<"type">>, <<"unsubscribed">>}],
    send_auto_reply(ToJID, JID1, Attrs);
do_auto_reply(ToJID, JID1, Item, subscribed) ->
    Attrs = lists:flatten(
              [{<<"type">>, <<"subscribed">>},
               item_name_to_xml(handle, Item#wocky_roster.contact_handle),
               item_name_to_xml(first_name, Item#wocky_roster.first_name),
               item_name_to_xml(last_name, Item#wocky_roster.last_name),
               item_name_to_xml(avatar, Item#wocky_roster.avatar)]),
    send_auto_reply(ToJID, JID1, Attrs).

send_auto_reply(ToJID, JID1, Attrs) ->
    ejabberd_router:route(ToJID, JID1, #xmlel{name = <<"presence">>,
                                              attrs = Attrs,
                                              children = []}).

%% roster_get_subscription_lists -------------------------------------

roster_get_subscription_lists_hook(Acc, User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Items = to_wocky_roster(?wocky_roster:get(LUser)),
    JID = jid:make(User, Server, <<>>),
    SubLists = fill_subscription_lists(JID, LServer, Items, [], []),
    mongoose_acc:put(subscription_lists, SubLists, Acc).

fill_subscription_lists(JID, LServer, [#wocky_roster{} = I | Is], F, T) ->
    J = I#wocky_roster.contact_jid,

    case I#wocky_roster.subscription of
        both ->
            fill_subscription_lists(JID, LServer, Is, [J | F], [J | T]);
        from ->
            fill_subscription_lists(JID, LServer, Is, [J | F], T);
        to ->
            fill_subscription_lists(JID, LServer, Is, F, [J | T]);
        _ ->
            fill_subscription_lists(JID, LServer, Is, F, T)
    end;
fill_subscription_lists(_, _, [], F, T) ->
    % Wocky auto-accepts subscriptions, so the 'pending' field is always
    % emtpy:
    {F, T, []}.

%% roster_get_jid_info -----------------------------------------------

roster_get_jid_info_hook(_Acc, User, _Server, JID) ->
    LUser = jid:nodeprep(User),
    {ContactUser, _, _} = JID,
    case ?wocky_id:'valid?'(ContactUser) andalso ?wocky_id:'valid?'(User) of
        true ->
            Item = to_wocky_roster(
                     LUser, JID,
                     ?wocky_roster:get(LUser, ContactUser)),
            {Item#wocky_roster.subscription, Item#wocky_roster.groups};
        false ->
            {none, []}
    end.


%% roster_get_versioning_feature -------------------------------------

roster_get_versioning_feature_hook(Acc, _Host) ->
    Feature = #xmlel{name = <<"ver">>,
                     attrs = [{<<"xmlns">>, ?NS_ROSTER_VER}]},
    [Feature | Acc].

%% local packet filter hook for user update messages
-type filter_packet() :: {ejabberd:jid(), ejabberd:jid(),
                          mongoose_acc:t(), jlib:xmlel()}.
-spec filter_local_packet_hook(filter_packet() | drop) ->
    filter_packet() | drop.
filter_local_packet_hook(P = {From, To, _Acc, Packet}) ->
    case handle_local_packet(From, To, Packet) of
        ok -> drop;
        {error, _} -> P
    end;
filter_local_packet_hook(Other) -> Other.

handle_local_packet(_From, To, Packet) ->
    do([error_m ||
        check_headline(Packet),
        UserChanged <- wocky_xml:get_subel(<<"user-changed">>, Packet),
        wocky_xml:check_namespace(?NS_USER, UserChanged),
        #xmlel{attrs = Attrs} <- wocky_xml:get_subel(<<"item">>, UserChanged),
        JID <- wocky_xml:get_attr(<<"jid">>, Attrs),
        send_update(To, JID)
       ]).

check_headline(#xmlel{name = <<"message">>, attrs = Attrs}) ->
    case xml:get_attr(<<"type">>, Attrs) of
        {value, <<"headline">>} -> ok;
        _ -> {error, not_headline}
    end;
check_headline(_) -> {error, not_headline}.

send_update(#jid{user = User, server = Server}, JID) ->
    Version = ?wocky_roster:version(User),
    #jid{luser = ContactUser} = jid:from_binary(JID),
    Item = to_wocky_roster(
             User, JID, ?wocky_roster:get(User, ContactUser)),
    Acc = push_item(User, Server, jid:make(<<>>, Server, <<>>),
                    Item, Item, Version),
    mongoose_acc:get(result, Acc).


%% hook for out-of-band roster modification --------------------------
roster_modified_hook(Acc, User, Server, JID) ->
    send_update(jid:make(User, Server, <<>>), JID),
    Acc.

%%%===================================================================
%%% Helper functions
%%%===================================================================

%% @spec (From::jid(), Item::roster()) -> ok
send_unsubscribing_presence(From, Item) ->
    LFrom = jid:to_bare(From),
    JID = jid:make(Item#wocky_roster.contact_jid),

    IsTo = case Item#wocky_roster.subscription of
               both -> true;
               to -> true;
               _ -> false
           end,
    IsFrom = case Item#wocky_roster.subscription of
                 both -> true;
                 from -> true;
                 _ -> false
             end,

    case {IsTo, IsFrom} of
        {true, _} -> send_presence_type(LFrom, JID, <<"unsubscribe">>);
        {_, true} -> send_presence_type(LFrom, JID, <<"unsubscribed">>);
        {false, false} -> ok
    end,
    ok.

send_presence_type(From, To, Type) ->
    ejabberd_router:route(From, To,
                          #xmlel{name = <<"presence">>,
                                 attrs = [{<<"type">>, Type}], children = []}).

item_to_xml(I = #wocky_roster{contact_jid = {ContactUser, _, _}}) ->
    Item = populate_extra_fields(I, ContactUser),
    #xmlel{
       name = <<"item">>,
       attrs = lists:flatten(
                 [item_jid_to_xml(Item#wocky_roster.contact_jid),
                  item_name_to_xml(name, Item#wocky_roster.name),
                  item_name_to_xml(handle, Item#wocky_roster.contact_handle),
                  item_name_to_xml(first_name, Item#wocky_roster.first_name),
                  item_name_to_xml(last_name, Item#wocky_roster.last_name),
                  item_name_to_xml(avatar, Item#wocky_roster.avatar),
                  item_date_to_xml(created_at, Item#wocky_roster.created_at),
                  item_sub_to_xml(Item#wocky_roster.subscription),
                  item_ask_to_xml(Item#wocky_roster.ask)]),
       children = [wocky_xml:cdata_el(<<"group">>, G) ||
                   G <- Item#wocky_roster.groups]
                  ++ [#xmlel{
                         name = <<"roles">>,
                         children = [wocky_xml:cdata_el(<<"role">>, R) ||
                                     R <- Item#wocky_roster.roles]}]
                  ++ Item#wocky_roster.xs
      }.

item_jid_to_xml(JID) ->
    {<<"jid">>, jid:to_binary(JID)}.

item_name_to_xml(_Key, nil) -> [];
item_name_to_xml(Key, Name) ->
    {erlang:atom_to_binary(Key, utf8), Name}.

item_date_to_xml(Key, DateTime) ->
    {erlang:atom_to_binary(Key, utf8), ?wocky_timestamp:to_string(DateTime)}.

item_sub_to_xml(Subscription) ->
    {<<"subscription">>, erlang:atom_to_binary(Subscription, utf8)}.

item_ask_to_xml(Ask)
  when Ask =:= out orelse Ask =:= both ->
    {<<"ask">>, <<"subscribe">>};
item_ask_to_xml(_) ->
    [].

push_item(User, Server, From,
          OldItem = #wocky_roster{},
          NewItem = #wocky_roster{}) ->
    ejabberd_sm:route(jid:make(<<"">>, <<"">>, <<"">>),
                      jid:make(User, Server, <<"">>),
                      {broadcast, {item,
                                   NewItem#wocky_roster.contact_jid,
                                   NewItem#wocky_roster.subscription,
                                   to_mim_roster(OldItem),
                                   to_mim_roster(NewItem)}}),
    push_item(User, Server, From, OldItem, NewItem,
              ?wocky_roster:version(jid:nodeprep(User))).

push_item(User, Server, From, OldItem, NewItem, RosterVersion) ->
    lists:foreach(fun (Resource) ->
                          push_item1(User, Server, Resource, From, NewItem,
                                     RosterVersion)
                  end,
                  ejabberd_sm:get_user_resources(User, Server)),

    ejabberd_hooks:run(
      roster_updated, Server, [User, Server, OldItem, NewItem]).


push_item1(User, Server, Resource, From, Item, RosterVersion) ->
    ResIQ = #iq{type = set, xmlns = ?NS_ROSTER,
                %% Roster push, calculate and include the version attribute.
                %% TODO: don't push to those who didn't load roster
                id = list_to_binary("push" ++ randoms:get_string()),
                sub_el = create_sub_el([item_to_xml(Item)], RosterVersion)},
    ejabberd_router:route(From, jid:make(User, Server, Resource),
                          jlib:iq_to_xml(ResIQ)).

to_mim_roster(#wocky_roster{
                 user = User,
                 server = Server,
                 contact_jid = ContactJID,
                 name = Name,
                 subscription = Subscription,
                 ask = Ask,
                 groups = Groups,
                 xs = XS}) ->
    #roster{
       usj = {User, Server, ContactJID},
       us = {User, Server},
       name = Name,
       subscription = Subscription,
       ask = Ask,
       groups = Groups,
       xs = XS}.

populate_extra_fields(Item, ContactUID) ->
    case ?wocky_repo:get_by(?wocky_user, [{id, ContactUID}]) of
        nil -> Item;
        #{handle     := Handle,
          avatar     := Avatar,
          first_name := FirstName,
          last_name  := LastName,
          roles      := Roles
         } ->
            Item#wocky_roster{
              contact_handle = Handle,
              avatar         = Avatar,
              first_name     = FirstName,
              last_name      = LastName,
              roles          = Roles
             }
    end.
