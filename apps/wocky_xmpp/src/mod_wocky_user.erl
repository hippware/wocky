%%% @copyright 2016+ Hippware, Inc.
%%% @doc Implementation module for user access IQ
%%% (https://github.com/hippware/tr-wiki/wiki/User).
%%%
-module(mod_wocky_user).

-compile({parse_transform, do}).
-compile({parse_transform, cut}).
-compile({parse_transform, fun_chain}).

-include("wocky.hrl").

-behaviour(gen_mod).

-export([
         start/2,
         stop/1,
         handle_iq/3
        ]).

%% Delay between sending result of a delete request and calling the
%% delete hook (which terminates the connection). This is needed to
%% ensure that the deleting user receives the IQ response before
%% the connection is dropped.
-define(USER_DELETE_DELAY, 2000).


%%--------------------------------------------------------------------
%% gen_mod interface
%%--------------------------------------------------------------------

-spec start(ejabberd:server(), list()) -> any().
start(Host, _Opts) ->
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_USER,
                                  ?MODULE, handle_iq, parallel).


-spec stop(ejabberd:server()) -> any().
stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_USER).


-spec handle_iq(From :: ejabberd:jid(),
                To :: ejabberd:jid(),
                IQ :: iq()) -> iq().
handle_iq(FromJID, ToJID, IQ = #iq{type = Type, sub_el = ReqEl}) ->
    case handle_request(IQ, FromJID, ToJID, Type, ReqEl) of
        {ok, Result} -> Result;
        {error, Stanza} -> make_error_response(IQ, Stanza)
    end.


%%--------------------------------------------------------------------
%% Common helpers
%%--------------------------------------------------------------------

handle_request(IQ, FromJID, #jid{lserver = LServer}, get,
               ReqEl = #xmlel{name = <<"get">>, children = Children}) ->
    do([error_m ||
        User <- get_user_node(ReqEl),
        UserJID <- get_user_jid(User, LServer),
        Relationship <- {ok, relationship(FromJID, UserJID)},
        Fields <- get_get_req_fields(Children, []),
        check_field_permissions(Relationship, Fields),
        XMLFields <- get_resp_fields(Fields, LServer, User),

        {ok, make_get_response_iq(XMLFields, IQ, User)}
       ]);

handle_request(IQ, FromJID, #jid{lserver = LServer}, get,
               #xmlel{name = <<"users">>, children = Children}) ->
    {ok,
     fun_chain:first(
       Children,
       get_users(),
       make_jids(),
       get_users_fields(FromJID, LServer),
       make_users_response_iq(IQ)
      )};

handle_request(IQ, FromJID, #jid{lserver = LServer}, set,
               ReqEl = #xmlel{name = <<"set">>, children = Children}) ->
    do([error_m ||
        User <- get_user_node(ReqEl),
        UserJID <- get_user_jid(User, LServer),
        validate_same_user(FromJID, UserJID),
        Fields <- get_set_req_fields(Children, []),
        set_user_fields(User, LServer, Fields),
        {ok, make_set_response_iq(IQ, User)}]);

handle_request(IQ, #jid{luser = LUser, lserver = LServer}, _ToJID, set,
               #xmlel{name = <<"delete">>}) ->
    ok = ?wocky_user:delete(LUser),
    {ok, _Ref} = timer:apply_after(?USER_DELETE_DELAY, ejabberd_hooks, run,
                                   [remove_user, LServer, [LUser, LServer]]),
    {ok, make_delete_response_iq(IQ)}.

make_jids(BJIDs) ->
    [{get_user_jid(B), B} || B <- BJIDs].

get_users_fields(JIDs, FromJID, LServer) ->
    lists:map(get_user_fields(FromJID, LServer, _), JIDs).

get_user_fields(_FromJID, _LServer, {Error = {error, _}, BinaryJID}) ->
    fun_chain:first(
      Error,
      handle_fields_error(),
      wrap_user_result(BinaryJID)
     );

get_user_fields(FromJID, LServer, {{ok, JID}, BinaryJID}) ->
    fun_chain:first(
      FromJID,
      relationship(JID),
      get_visible_fields(),
      get_resp_fields(LServer, JID#jid.luser),
      handle_fields_error(),
      wrap_user_result(BinaryJID)
     ).

get_user_node(ReqEl) ->
    case exml_query:attr(ReqEl, <<"node">>) of
        <<"user/", User/binary>> when byte_size(User) > 0 ->
            {ok, User};
        undefined ->
            not_valid("Missing node attribute");
        _ ->
            not_valid("Malformed node attribute")
    end.

get_users(Children) ->
    lists:foldl(get_user(_, _), [], Children).

get_user(UserEl = #xmlel{name = <<"user">>}, Acc) ->
    case exml_query:attr(UserEl, <<"jid">>) of
        undefined -> Acc;
        X -> [X | Acc]
    end;
get_user(_, Acc) -> Acc.

get_user_jid(BJID) ->
    check_jid_result(jid:from_binary(BJID)).

get_user_jid(User, Server) ->
    check_jid_result(jid:make(User, Server, <<>>)).

check_jid_result(error) ->
    {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Invalid JID">>)};
check_jid_result(#jid{luser = <<>>}) ->
    {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Missing user">>)};
check_jid_result(JID = #jid{luser = LUser}) ->
    case ?wocky_id:'valid?'(LUser) of
       true -> {ok, JID};
       false -> {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Invalid user">>)}
    end.

validate_same_user(FromJID, UserJID) ->
    case relationship(FromJID, UserJID) of
        self -> ok;
        _ -> {error, ?ERRT_FORBIDDEN(?MYLANG, <<"Can only modify yourself">>)}
    end.

relationship(FromJID, UserJID) ->
    case jid:are_bare_equal(FromJID, UserJID) of
        true -> self;
        false -> relationship_not_self(FromJID, UserJID)
    end.

relationship_not_self(#jid{luser = FromUser}, #jid{luser = User}) ->
    case ?wocky_roster_item:has_contact(User, FromUser) of
        true -> friend;
        false -> stranger
    end.


get_field(Var) ->
    case lists:keyfind(binary_to_list(Var), 1, fields()) of
        false ->
            not_valid(["Unknown field name: ", Var]);
        Field ->
            {ok, Field}
    end.

fields() ->
     % Field name     % Type    % Visibility % Access   % Accessor
    [{"jid",          "jid",    public,      read_only,
      fun(#{id := LUser, server := LServer}) ->
              jid:to_binary(jid:make(LUser, LServer, <<>>)) end},
     {"user",         "uuid",   public,      read_only,
      fun(#{id := User}) -> User end},
     {"server",       "string", public,      read_only, default},
     {"handle",       "string", public,      write,     default},
     {"phone_number", "string", private,     read_only, default},
     {"avatar",       "file",   public,      write,     default},
     {"first_name",   "string", public,      write,     default},
     {"last_name",    "string", public,      write,     default},
     {"email",        "string", private,     write,     default},
     {"external_id",  "string", private,     read_only, default},
     {"bots+size",    "int",    public,      read_only,
      fun(User) ->
              integer_to_binary(?wocky_user:bot_count(User)) end},
     {"followers+size", "int",  public,      read_only,
      fun(#{id := LUser}) ->
              integer_to_binary(
                length(?wocky_roster_item:followers(LUser))) end},
     {"followed+size", "int",   public,      read_only,
      fun(#{id := LUser}) ->
              integer_to_binary(
                length(?wocky_roster_item:following(LUser))) end}
    ].


field_name(Field) -> element(1, Field).
field_type(Field) -> element(2, Field).
field_visibility(Field) -> element(3, Field).
field_access(Field) -> element(4, Field).
field_accessor(Field) -> element(5, Field).

%%--------------------------------------------------------------------
%% GET-specific helpers
%%--------------------------------------------------------------------

get_get_req_fields([], []) ->
    {ok, fields()};
get_get_req_fields([], Fields) ->
    {ok, Fields};
get_get_req_fields([El = #xmlel{name = <<"field">>} | Tail], Acc) ->
    case exml_query:attr(El, <<"var">>) of
        undefined ->
            not_valid(<<"Missing var attribute on field">>);
        Var ->
            add_get_req_field(Tail, Acc, Var)
    end;
get_get_req_fields([_ | Tail], Acc) ->
    get_get_req_fields(Tail, Acc).


add_get_req_field(RemainingFields, Acc, Var) ->
    case get_field(Var) of
        {error, E} ->
            {error, E};
        {ok, Field} ->
            get_get_req_fields(RemainingFields, [Field | Acc])
    end.


check_field_permissions(Relationship, Fields) ->
    case lists:all(fun(F) -> is_visible(Relationship, field_visibility(F)) end,
                   Fields) of
        true -> ok;
        false -> {error, ?ERRT_FORBIDDEN(?MYLANG, <<"No access to field">>)}
    end.


get_visible_fields(Relationship) ->
    lists:filter(fun(F) -> is_visible(Relationship, field_visibility(F)) end,
                 fields()).

is_visible(self, _)        -> true;
is_visible(_,    private)  -> false;
is_visible(_,    public)   -> true.

get_resp_fields(Fields, _LServer, LUser) ->
    case ?wocky_repo:get(?wocky_user, LUser) of
        nil ->
            {error, ?ERRT_ITEM_NOT_FOUND(?MYLANG, <<"User not found">>)};
        Row ->
            {ok, build_resp_fields(Row, Fields)}
    end.


build_resp_fields(Row, Fields) ->
    lists:foldl(
      fun(Field, Acc) -> [build_resp_field(Row, Field) | Acc] end,
      [], Fields).


build_resp_field(Row, Field) ->
    #xmlel{name = <<"field">>,
           attrs = [{<<"var">>,  list_to_binary(field_name(Field))},
                    {<<"type">>, list_to_binary(field_type(Field))}],
           children = [value_element(extract(field_name(Field), Row,
                                             field_accessor(Field)))]}.


extract(Key, Row, default) ->
    maps:get(list_to_existing_atom(Key), Row);

extract(_, Row, Fun) -> Fun(Row).


value_element(Value) ->
    #xmlel{name = <<"value">>,
           children = [#xmlcdata{content = null_to_bin(Value)}]}.


make_get_response_iq(Fields, IQ, User) ->
    IQ#iq{type = result,
          sub_el = #xmlel{name = <<"fields">>,
                          attrs = response_attrs(User),
                          children = Fields}}.


make_users_response_iq(Fields, IQ) ->
    IQ#iq{type = result,
          sub_el = #xmlel{name = <<"users">>,
                          children = Fields}}.


make_set_response_iq(IQ, User) ->
    IQ#iq{type = result,
          sub_el = #xmlel{name = <<"setResponse">>,
                          attrs = response_attrs(User)}}.

make_delete_response_iq(IQ) ->
    IQ#iq{type = result, sub_el = []}.

response_attrs(User) ->
    [{<<"xmlns">>, ?NS_USER},
     {<<"node">>, <<"user/", User/binary>>}].


make_error_response(IQ, ErrStanza) ->
    ok = lager:warning("Error on user IQ request: ~p", [ErrStanza]),
    IQ#iq{type = error, sub_el = ErrStanza}.

wrap_user_result(Result, BJID) ->
    #xmlel{name = <<"user">>,
           attrs = [{<<"jid">>, BJID}],
           children = Result}.

handle_fields_error({ok, XML}) ->
    XML;
handle_fields_error({error, Stanza}) ->
    [Stanza].

null_to_bin(nil) -> <<"">>;
null_to_bin(X) -> X.


%%--------------------------------------------------------------------
%% SET helpers
%%--------------------------------------------------------------------

get_set_req_fields([], Acc) ->
    {ok, Acc};

get_set_req_fields([El = #xmlel{name = <<"field">>} | Tail], Acc) ->
    case get_set_req_field(El) of
        {ok, Result} -> get_set_req_fields(Tail, [Result | Acc]);
        {error, E} -> {error, E}
    end;

get_set_req_fields([_ | Tail], Acc) ->
    get_set_req_fields(Tail, Acc).


get_set_req_field(#xmlel{attrs = Attrs, children = Children}) ->
    do([error_m ||
        Var <- get_var(Attrs),
        Type <- get_type(Attrs),
        Field <- get_field(Var),
        check_type(Field, Type),
        check_editability(Field),
        Value <- get_value(Children, Field),
        {ok, {Var, Value}}]).


get_var(Attrs) ->
    get_attr(<<"var">>, Attrs).


get_type(Attrs) ->
    get_attr(<<"type">>, Attrs).


get_attr(Name, Attrs) ->
    case proplists:get_value(Name, Attrs) of
        undefined ->
            not_valid(["Missing ", Name, " attribute on field"]);
        Value ->
            {ok, Value}
    end.


check_type({Name, StrType, _, _, _}, Type) ->
    case binary_to_list(Type) of
        StrType ->
            ok;
        _ ->
            not_valid(["Bad type on field ", Name, ": ", Type])
    end.


check_editability(Field) ->
    case field_access(Field) of
        read_only ->
            {error,
             ?ERRT_FORBIDDEN(?MYLANG,
                             iolist_to_binary(
                               ["Field ", field_name(Field),
                                " is read-only"]))};
        write ->
            ok
    end.


get_value([#xmlel{name = <<"value">>,
                  children = [#xmlcdata{content = Value}]} | _], _Field) ->
    {ok, Value};

get_value([#xmlel{name = <<"value">>} | _], Field) ->
    % Missing value content - treat the same as an absent value field
    get_value([], Field);

get_value([], Field) ->
    not_valid(["Missing value on ", field_name(Field)]);

get_value([_ | Tail], Field) ->
    get_value(Tail, Field).


set_user_fields(LUser, LServer, Fields) ->
    Row = build_row(Fields),
    case maps:size(Row) of
        0 -> ok;
        _ -> update_user(LUser, LServer, Row)
    end.

build_row(Fields) ->
    lists:foldl(fun({Name, Value}, Acc) ->
                      case lists:member(Name, valid_user_fields()) of
                         true ->
                            NameAtom = binary_to_atom(Name, utf8),
                            Acc#{NameAtom => Value};
                         false ->
                            Acc
                      end
                end,
                #{},
                Fields).

valid_user_fields() -> ?wocky_user:valid_update_fields().

update_user(LUser, LServer, Row) ->
   case ?wocky_user:update(LUser, Row) of
       ok ->
         update_roster_contacts(LUser),
         ejabberd_hooks:run(wocky_user_updated, LServer, [LUser, LServer]),
         ok;

      {error, #{'valid?' := false} = Changeset} ->
           handle_validation_errors(?wocky_errors:to_map(Changeset));

      {error, Reason} ->
         Message = iolist_to_binary(
                     io_lib:format("Unexpected error: ~p", [Reason])),
         {error, ?ERRT_INTERNAL_SERVER_ERROR(?MYLANG, Message)}
   end.

handle_validation_errors(#{handle := <<"unavailable">>}) ->
    error_with_child(
      ?ERRT_CONFLICT(?MYLANG, <<"Could not set handle - already in use">>),
      #xmlel{name = <<"field">>, attrs = [{<<"var">>, <<"handle">>}]});
handle_validation_errors(Errors) ->
    not_valid(?wocky_errors:render_errors(Errors)).

not_valid(Message) ->
    El = #xmlel{children = Children} =
    jlib:stanza_errort(<<"500">>, <<"modify">>, <<"undefined-condition">>,
                       ?MYLANG, iolist_to_binary(Message)),
    Stanza = El#xmlel{children = [#xmlel{name = <<"not-valid">>,
                                         attrs = [{<<"xmlns">>, ?NS_ERRORS}]}
                                  | Children]},
    {error, Stanza}.

error_with_child(Stanza = #xmlel{children = Children}, ExtraChild) ->
    {error, Stanza#xmlel{children = [ExtraChild | Children]}}.

update_roster_contacts(LUser) ->
    Users = ?wocky_roster_item:find_users_with_contact(LUser),
    lists:foreach(bump_roster_version(LUser, _), Users).

bump_roster_version(LUser, #{id := ContactID}) ->
    ?wocky_roster_item:bump_version(LUser, ContactID).
