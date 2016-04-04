%%% @copyright 2016+ Hippware, Inc.
%%% @doc Implementation module for user access IQ
%%% (https://github.com/hippware/tr-wiki/wiki/User).
%%%
-module(mod_wocky_user).

-export([
   start/2,
   stop/1,
   handle_iq/3
        ]).

-define(USER_NS, <<"hippware.com/hxep/user">>).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").

-behaviour(gen_mod).

-compile({parse_transform, do}).

%%--------------------------------------------------------------------
%% gen_mod interface
%%--------------------------------------------------------------------

-spec start(ejabberd:server(), list()) -> any().
start(Host, _Opts) ->
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?USER_NS,
                                  ?MODULE, handle_iq, parallel).


-spec stop(ejabberd:server()) -> any().
stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?USER_NS).


-spec handle_iq(From :: ejabberd:jid(),
                To :: ejabberd:jid(),
                IQ :: iq()) -> iq().
handle_iq(FromJID, ToJID, IQ = #iq{type = Type, sub_el = ReqEl}) ->
    case handle_request(IQ, FromJID, ToJID, Type, ReqEl) of
        {ok, Result} -> Result;
        {error, {Error, Stanza}} -> make_error_response(IQ, Error, Stanza)
    end.


%%--------------------------------------------------------------------
%% Common helpers
%%--------------------------------------------------------------------

handle_request(IQ, FromJID, ToJID, get,
               ReqEl = #xmlel{name = <<"get">>, children = Children}) ->
    do([error_m ||
        User <- get_user(ReqEl),
        validate_user(User),
        Relationship <- {ok, relationship(FromJID, User, ToJID)},
        Fields <- get_get_req_fields(Children, []),
        check_field_permissions(Relationship, Fields),
        XMLFields <- get_resp_fields(ToJID, User, Fields),
        {ok, make_response_iq(IQ, User, XMLFields)}]);

handle_request(IQ, FromJID, ToJID = #jid{lserver = LServer}, set,
               ReqEl = #xmlel{name = <<"set">>, children = Children}) ->
    do([error_m ||
        User <- get_user(ReqEl),
        validate_user(User),
        validate_same_user(FromJID, User, ToJID),
        Fields <- get_set_req_fields(Children, []),
        check_duplicate_unique_fields(Fields),
        set_unique_fields(User, LServer, Fields),
        set_other_fields(User, Fields),
        {ok, make_response_iq(IQ, User, [])}]).


get_user(ReqEl) ->
    case exml_query:attr(ReqEl, <<"node">>) of
        <<"user/", User/binary>> ->
            {ok, User};
        undefined ->
            {error, {"Missing node attribute", ?ERR_BAD_REQUEST}};
        _ ->
            {error, {"Malformed node attribute", ?ERR_BAD_REQUEST}}
    end.


validate_user(User) ->
    case wocky_db:is_valid_id(User) of
        true -> ok;
        false -> {error, {"Invalid user ID", ?ERR_BAD_REQUEST}}
    end.


validate_same_user(FromJID, User, ToJID) ->
    case relationship(FromJID, User, ToJID) of
        self -> ok;
        _ -> {error, {"Can only modify yourself", ?ERR_FORBIDDEN}}
    end.


relationship(FromJID, User, ToJID) ->
    TargetJID = jid:make(User, ToJID#jid.lserver, <<>>),
    case jid:are_bare_equal(FromJID, TargetJID) of
        true -> self;
        false -> relationship(FromJID, TargetJID)
    end.

relationship(FromJID, TargetJID) ->
    case wocky_db_roster:has_contact(TargetJID, FromJID) of
        true -> friend;
        false -> stranger
    end.


get_field(Var) ->
    case lists:keyfind(binary_to_list(Var), 1, fields()) of
        false ->
            {error, {["Unknown field name: ", Var], ?ERR_BAD_REQUEST}};
        Field ->
            {ok, Field}
    end.


fields() ->
     % Field name    % Type    % Visibility % Access   % Accessor
    [{"jid",         "jid",    public,      read_only,
      fun(#{user := LUser, server := LServer}) ->
              jid:to_binary(jid:make(LUser, LServer, <<>>)) end},
     {"uuid",        "uuid",   public,      read_only, default},
     {"server",      "string", public,      read_only, default},
     {"handle",      "string", public,      write,     default},
     {"phoneNumber", "string", private,     rest_only, default},
     {"status",      "int",    private,     read_only, fun(_) -> <<"0">> end},
     {"avatar",      "file",   public,      write,     default},
     {"firstName",   "string", public,      write,     default},
     {"lastName",    "string", public,      write,     default},
     {"email",       "string", private,     write,     default},
     {"userID",      "string", private,     rest_only, default}
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
            {error, {"Missing var attribute on field", ?ERR_BAD_REQUEST}};
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
        false -> {error, {"No access to field", ?ERR_FORBIDDEN}}
    end.


is_visible(self,     _)      -> true;
is_visible(friend,   private)-> false;
is_visible(friend,   _)      -> true;
is_visible(stranger, public) -> true;
is_visible(stranger, _)      -> false.


get_resp_fields(ToJID, LUser, Fields) ->
    LServer = ToJID#jid.lserver,
    case wocky_db_user:get_user_data(LUser, LServer) of
        not_found ->
            {error, {"User not found", ?ERR_ITEM_NOT_FOUND}};
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


extract(Name, Row, default) ->
    Key = client_db_map:client_to_db(list_to_atom(Name)),
    maps:get(Key, Row);

extract(_, Row, Fun) -> Fun(Row).


value_element(Value) ->
    #xmlel{name = <<"value">>,
           children = [#xmlcdata{content = null_to_bin(Value)}]}.


make_response_iq(IQ, User, Fields) ->
    IQ#iq{type = result,
          sub_el = #xmlel{name = <<"fields">>,
                          attrs = [{<<"xmlns">>, ?USER_NS},
                                   {<<"node">>, <<"user/", User/binary>>}],
                          children = Fields}}.


make_error_response(IQ, Error, ErrStanza) ->
    ok = lager:warning("Error on user IQ request: ~p", [Error]),
    IQ#iq{type = error, sub_el = ErrStanza}.


null_to_bin(null) -> <<"">>;
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
        Value <- get_value(Children),
        {ok, {Var, Value}}]).


get_var(Attrs) ->
    get_attr(<<"var">>, Attrs).


get_type(Attrs) ->
    get_attr(<<"type">>, Attrs).


get_attr(Name, Attrs) ->
    case proplists:get_value(Name, Attrs) of
        undefined ->
            {error, {["Missing ", Name, " attribute on field"],
                     ?ERR_BAD_REQUEST}};
        Value ->
            {ok, Value}
    end.


check_type({Name, StrType, _, _, _}, Type) ->
    case binary_to_list(Type) of
        StrType ->
            ok;
        _ ->
            {error, {["Bad type on field ", Name, ": ", Type],
                     ?ERR_BAD_REQUEST}}
    end.


check_editability(Field) ->
    case field_access(Field) of
        read_only ->
            {error, {["Field ", field_name(Field), " is read-only"],
                     ?ERR_FORBIDDEN}};
        rest_only ->
            {error,
             {["Field ", field_name(Field),
               " may only be changed through REST interface"],
             ?ERR_FORBIDDEN}};
        write ->
            ok
    end.


get_value([#xmlel{name = <<"value">>,
                  children = [#xmlcdata{content = Value}]} | _]) ->
    {ok, Value};

get_value([#xmlel{name = <<"value">>} | _]) ->
    {error, {"Missing value", ?ERR_BAD_REQUEST}};

get_value([]) ->
    {error, {"Missing value", ?ERR_BAD_REQUEST}};

get_value([_ | Tail]) ->
    get_value(Tail).


check_duplicate_unique_fields(_Fields) -> ok.


set_unique_fields(LUser, LServer, Fields) ->
    case proplists:get_value(<<"handle">>, Fields) of
        undefined -> ok;
        Handle -> set_handle(LUser, LServer, Handle)
    end.


set_handle(LUser, LServer, Handle) ->
    case wocky_db_user:maybe_set_handle(LUser, LServer, Handle) of
        true -> ok;
        false -> {error, {"Could not set handle - already in use",
                          ?ERR_CONFLICT}}
    end.


set_other_fields(LUser, Fields) ->
    OtherFields = proplists:delete(<<"handle">>, Fields),
    Row = build_row(OtherFields),
    case maps:size(Row) of
        0 -> ok;
        _ -> ok = wocky_db_user:update_user(Row#{user => LUser})
    end.


build_row(Fields) ->
    lists:foldl(fun({Name, Value}, Acc) ->
                        NameAtom = binary_to_atom(Name, utf8),
                        DBField = client_db_map:client_to_db(NameAtom),
                        Acc#{DBField => Value}
                end,
                #{},
                Fields).
