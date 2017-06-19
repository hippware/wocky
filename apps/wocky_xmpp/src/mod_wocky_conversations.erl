%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module implementing conversation list querying
%%% See https://github.com/hippware/tr-wiki/wiki/Conversations-List
%%%

-module(mod_wocky_conversations).

-compile({parse_transform, cut}).

-include("wocky.hrl").

-behaviour(gen_mod).

%% gen_mod handlers
-export([start/2, stop/1]).
-export([handle_iq/3]).
-export([archive_message_hook/9]).

-define(DEFAULT_MAX, 50).


start(Host, Opts) ->
    wocky_util:set_config_from_opt(default_max,
                                   conv_max,
                                   ?DEFAULT_MAX,
                                   Opts),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_CONVERSATIONS_LEGACY,
                                  ?MODULE, handle_iq, parallel),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_CONVERSATIONS,
                                  ?MODULE, handle_iq, parallel),
    ejabberd_hooks:add(mam_archive_message, Host, ?MODULE,
                       archive_message_hook, 50).

stop(Host) ->
    ejabberd_hooks:delete(mam_archive_message, Host, ?MODULE,
                          archive_message_hook, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
                                     ?NS_CONVERSATIONS),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
                                     ?NS_CONVERSATIONS_LEGACY).

%%%===================================================================
%%% IQ packet handling
%%%===================================================================

-spec handle_iq(From :: ejabberd:jid(),
                To :: ejabberd:jid(),
                IQ :: iq()) -> iq().
handle_iq(From, To, IQ) ->
    handle_iq_type(From, To, IQ).

%% TODO: Remove the 'set' variant once the client is updated
handle_iq_type(From, _To,
               IQ = #iq{type = set,
                        sub_el = #xmlel{name = <<"query">>}}) ->
    get_conversations_response(From, IQ);
handle_iq_type(From, _To,
               IQ = #iq{type = get,
                        sub_el = #xmlel{name = <<"query">>}}) ->
    get_conversations_response(From, IQ);
handle_iq_type(_From, _To, _IQ) ->
    {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Invalid query">>)}.

%%%===================================================================
%%% mam_archive_message callback
%%%===================================================================

-spec archive_message_hook(Result :: any(),
                           Host   :: ejabberd:server(),
                           MessID :: mod_mam:message_id(),
                           ArcID  :: mod_mam:archive_id(),
                           LocJID :: ejabberd:jid(),
                           RemJID :: ejabberd:jid(),
                           SrcJID :: ejabberd:jid(),
                           Dir    :: incoming | outgoing,
                           Packet :: exml:element()
                          ) -> ok.
archive_message_hook(_Result, _Host, MessID, _ArcID,
                LocJID, RemJID, _SrcJID, Dir, Packet) ->
  ok = ?wocky_conversation:put(MessID,
                               LocJID#jid.luser,
                               jid:to_binary(jid:to_bare(RemJID)),
                               exml:to_binary(Packet),
                               Dir =:= outgoing
                              ).

%%%===================================================================
%%% Conversation retrieval
%%%===================================================================

get_conversations_response(From, IQ = #iq{sub_el = SubEl}) ->
    RSM = jlib:rsm_decode(SubEl),
    RSM2 = id_to_int(cap_max(RSM)),
    {Conversations, RSMOut} = get_conversations(From, RSM2),
    create_response(IQ, Conversations, RSMOut).

get_conversations(From, RSMIn) ->
    {Records, RSMOut = #rsm_out{first = First, last = Last}} =
    ?wocky_rsm_helper:rsm_query(RSMIn,
                                ?wocky_conversation:with_user(From#jid.luser),
                                id,
                                {desc, updated_at}),
    {Records,
     RSMOut#rsm_out{first = maybe_integer_to_binary(First),
                    last = maybe_integer_to_binary(Last)}}.

%%%===================================================================
%%% Helpers
%%%===================================================================

cap_max(none) ->
    #rsm_in{max = max_results()};
cap_max(RSM = #rsm_in{max = Max}) ->
    RSM#rsm_in{max = min(Max, max_results())}.

id_to_int(RSM = #rsm_in{id = ID})
  when ID =:= undefined orelse ID =:= <<>> ->
    RSM#rsm_in{id = undefined};
id_to_int(RSM = #rsm_in{id = ID}) ->
    RSM#rsm_in{id = wocky_util:default_bin_to_integer(ID, 0)}.

max_results() ->
    ejabberd_config:get_local_option(conv_max).

create_response(IQ, Conversations, RSMOut) ->
    IQ#iq{type = result,
          sub_el = [#xmlel{name = <<"query">>,
                           attrs = [{<<"xmlns">>, ?NS_CONVERSATIONS}],
                           children =
                           conversations_xml(Conversations) ++
                           jlib:rsm_encode(RSMOut)
                          }]}.

conversations_xml(Conversations) ->
    [conversation_xml(C) || C <- Conversations].

conversation_xml(Conversation = #{id := ID}) ->
    #xmlel{name = <<"item">>,
           attrs = [{<<"id">>, integer_to_binary(ID)}],
           children = conversation_data_xml(Conversation)}.

conversation_data_xml(Conversation) ->
    Elements = [message, other_jid, updated_at, outgoing],
    [conversation_element(E, maps:get(E, Conversation)) || E <- Elements].

conversation_element(message, Message) ->
    case exml:parse(Message) of
        {ok, XML} -> XML;
        {error, _} -> #xmlel{name = <<"message">>,
                             children = [#xmlcdata{content = <<"error">>}]}
    end;

conversation_element(updated_at, Time) ->
    wocky_xml:cdata_el(<<"timestamp">>, ?wocky_timestamp:to_string(Time));

conversation_element(E, V) ->
    wocky_xml:cdata_el(atom_to_binary(E, utf8), to_binary(V)).

to_binary(B) when is_binary(B) -> B;
to_binary(I) when is_integer(I) -> integer_to_binary(I);
to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8).

maybe_integer_to_binary(undefined) -> undefined;
maybe_integer_to_binary(I) when is_integer(I) -> integer_to_binary(I).
