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

-ignore_xref([{handle_iq, 3}]).

-define(USER_NS, <<"hippware.com/hxep/user">>).
-define(ERROR_NS, <<"hippware.com/hxep/errors">>).


-include("wocky.hrl").
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
        {error, Stanza} -> make_error_response(IQ, Stanza)
    end.


%%--------------------------------------------------------------------
%% Common helpers
%%--------------------------------------------------------------------

handle_request(IQ, FromJID, ToJID, get,
               ReqEl = #xmlel{name = <<"get">>, children = Children}) ->
    do([error_m ||
        User <- get_user(ReqEl),
        Relationship <- {ok, relationship(FromJID, User, ToJID)},
        Fields <- get_get_req_fields(Children, []),
        check_field_permissions(Relationship, Fields),
        XMLFields <- get_resp_fields(ToJID, User, Fields),
        {ok, make_get_response_iq(IQ, User, XMLFields)}]);

handle_request(IQ, FromJID, ToJID = #jid{lserver = LServer}, set,
               ReqEl = #xmlel{name = <<"set">>, children = Children}) ->
    do([error_m ||
        User <- get_user(ReqEl),
        validate_same_user(FromJID, User, ToJID),
        Fields <- get_set_req_fields(Children, []),
        set_user_fields(User, LServer, Fields),
        {ok, make_set_response_iq(IQ, User)}]);

handle_request(IQ, #jid{luser = LUser, lserver = LServer}, _ToJID, set,
               #xmlel{name = <<"delete">>}) ->
    ok = wocky_db_user:remove_user(LUser, LServer),
    {ok, _Ref} = timer:apply_after(?USER_DELETE_DELAY, ejabberd_hooks, run,
                                   [remove_user, LServer, [LUser, LServer]]),
    {ok, make_delete_response_iq(IQ)}.

get_user(ReqEl) ->
    case exml_query:attr(ReqEl, <<"node">>) of
        <<"user/", User/binary>> ->
            {ok, User};
        undefined ->
            not_valid("Missing node attribute");
        _ ->
            not_valid("Malformed node attribute")
    end.


validate_same_user(FromJID, User, ToJID) ->
    case relationship(FromJID, User, ToJID) of
        self -> ok;
        _ -> {error, ?ERRT_FORBIDDEN(?MYLANG, <<"Can only modify yourself">>)}
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
            not_valid(["Unknown field name: ", Var]);
        Field ->
            {ok, Field}
    end.

get_phone_number(#{user := LUser, server := LServer}) ->
   case wocky_db_user:get_phone_number(LUser, LServer) of
      not_found -> <<>>;
      PhoneNumber -> PhoneNumber
   end.

fields() ->
     % Field name     % Type    % Visibility % Access   % Accessor
    [{"jid",          "jid",    public,      read_only,
      fun(#{user := LUser, server := LServer}) ->
              jid:to_binary(jid:make(LUser, LServer, <<>>)) end},
     {"user",         "uuid",   public,      read_only, default},
     {"server",       "string", public,      read_only, default},
     {"handle",       "string", public,      write,     default},
     {"phone_number", "string", private,     rest_only, fun get_phone_number/1},
     {"avatar",       "file",   public,      write,     default},
     {"first_name",   "string", public,      write,     default},
     {"last_name",    "string", public,      write,     default},
     {"email",        "string", private,     write,     default},
     {"external_id",  "string", private,     rest_only, default}
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


is_visible(self,     _)      -> true;
is_visible(friend,   private)-> false;
is_visible(friend,   _)      -> true;
is_visible(stranger, public) -> true;
is_visible(stranger, _)      -> false.


get_resp_fields(ToJID, LUser, Fields) ->
    LServer = ToJID#jid.lserver,
    case wocky_db_user:find_user(LUser, LServer) of
        not_found ->
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


make_get_response_iq(IQ, User, Fields) ->
    IQ#iq{type = result,
          sub_el = #xmlel{name = <<"fields">>,
                          attrs = response_attrs(User),
                          children = Fields}}.


make_set_response_iq(IQ, User) ->
    IQ#iq{type = result,
          sub_el = #xmlel{name = <<"setResponse">>,
                          attrs = response_attrs(User)}}.

make_delete_response_iq(IQ) ->
    IQ#iq{type = result, sub_el = []}.

response_attrs(User) ->
    [{<<"xmlns">>, ?USER_NS},
     {<<"node">>, <<"user/", User/binary>>}].


make_error_response(IQ, ErrStanza) ->
    ok = lager:warning("Error on user IQ request: ~p", [ErrStanza]),
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
        Value <- get_value(Children, Field),
        check_valid_value(field_name(Field), field_type(Field), Value),
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
        rest_only ->
            Error = iolist_to_binary(
                      ["Field ", field_name(Field),
                       " may only be changed through REST interface"]),
            {error, ?ERRT_FORBIDDEN(?MYLANG, Error)};
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


check_valid_value("email", "string", Value) ->
    case z_email_utils:is_email(Value) of
        true -> ok;
        false -> not_valid("Invalid email address")
    end;

check_valid_value(_, "file", Value) ->
    Server = wocky_app:server(),
    case (tros:parse_url(Value)) of
        {ok, {Server, File}} -> check_file(File);
        {ok, {OtherServer, _}} -> not_valid(["Server ", OtherServer,
                                             " is not local"]);
        {error, invalid_url} -> not_valid("Invalid file URL")
    end;

check_valid_value(_, _, _) ->
    ok.


check_file(File) ->
    case wocky_db:is_valid_id(File) of
        true -> ok;
        false -> not_valid("Invalid file name (must be UUID)")
    end.


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

valid_user_fields() ->
    [<<"handle">>, <<"avatar">>, <<"first_name">>,
     <<"last_name">>, <<"email">>].

update_user(LUser, LServer, Row) ->
   case wocky_db_user:update_user(LUser, LServer, Row) of
       ok ->
         ok;

      {error, duplicate_handle} ->
         error_with_child(
           ?ERRT_CONFLICT(?MYLANG, <<"Could not set handle - already in use">>),
           #xmlel{name = <<"field">>, attrs = [{<<"var">>, <<"handle">>}]});

      {error, Reason} ->
         Message = iolist_to_binary(["Unexpected error: ",
                                     atom_to_list(Reason)]),
         {error, ?ERRT_INTERNAL_SERVER_ERROR(?MYLANG, Message)}
   end.

not_valid(Message) ->
    El = #xmlel{children = Children} =
    jlib:stanza_errort(<<"500">>, <<"modify">>, <<"undefined-condition">>,
                       ?MYLANG, iolist_to_binary(Message)),
    Stanza = El#xmlel{children = [#xmlel{name = <<"not-valid">>,
                                         attrs = [{<<"xmlns">>, ?ERROR_NS}]}
                                  | Children]},
    {error, Stanza}.

error_with_child(Stanza = #xmlel{children = Children}, ExtraChild) ->
    {error, Stanza#xmlel{children = [ExtraChild | Children]}}.
