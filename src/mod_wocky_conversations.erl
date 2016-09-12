%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module implementing conversation list querying
%%% See https://github.com/hippware/tr-wiki/wiki/Conversations-List
%%%

-module(mod_wocky_conversations).

-behaviour(gen_mod).

-include("wocky.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").

-compile({parse_transform, do}).

%% gen_mod handlers
-export([start/2, stop/1]).

-export([handle_iq/3]).

-ignore_xref([{handle_iq, 3}]).

-define(DEFAULT_MAX, 50).

start(Host, Opts) ->
    wocky_util:set_config_from_opt(default_max,
                                   conv_max,
                                   ?DEFAULT_MAX,
                                   Opts),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_CONVERSATIONS,
                                  ?MODULE, handle_iq, parallel).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_CONVERSATIONS).

%%%===================================================================
%%% IQ packet handling
%%%===================================================================

-spec handle_iq(From :: ejabberd:jid(),
                To :: ejabberd:jid(),
                IQ :: iq()) -> iq().
handle_iq(From, To, IQ) ->
    handle_iq_type(From, To, IQ).

handle_iq_type(From, _To,
               IQ = #iq{type = set,
                        sub_el = #xmlel{name = <<"query">>}}) ->
    get_conversations_response(From, IQ);
handle_iq_type(_From, _To, _IQ) ->
    {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Invalid query">>)}.

%%%===================================================================
%%% Conversation retrieval
%%%===================================================================

get_conversations_response(From, IQ = #iq{sub_el = SubEl}) ->
    RSM = jlib:rsm_decode(SubEl),
    RSM2 = id_to_int(cap_max(RSM)),
    {Conversations, RSMOut} = get_conversations(From, RSM2),
    create_response(IQ, Conversations, RSMOut).

get_conversations(From, RSMIn) ->
    UserJID = wocky_util:archive_jid(From),
    Rows = wocky_db:select(wocky_app:server(),
                           conversation, all, #{user_jid => UserJID}),
    ResultWithTimes = [R#{timestamp => uuid:get_v1_time(
                                         uuid:string_to_uuid(T))} ||
                       R = #{time := T} <- Rows],
    SortedResult = sort_result(ResultWithTimes),
    rsm_util:filter_with_rsm(SortedResult, RSMIn).

%%%===================================================================
%%% Helpers
%%%===================================================================

sort_result(Rows) ->
    lists:sort(fun sort_by_id/2, Rows).

% Sort the most recent to the front
sort_by_id(#{id := ID1}, #{id := ID2}) ->
    ID1 > ID2.

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

conversation_xml(Conversation) ->
    #xmlel{name = <<"item">>,
           attrs = [{<<"id">>, integer_to_binary(maps:get(id, Conversation))}],
           children = conversation_data_xml(Conversation)}.

conversation_data_xml(Conversation) ->
    Elements = [other_jid, timestamp, outgoing],
    [message_element(Conversation) |
    [conversation_element(E, Conversation) || E <- Elements]].

message_element(C) ->
    case exml:parse(maps:get(message, C)) of
        {ok, XML} -> XML;
        {error, _} -> #xmlel{name = <<"message">>,
                             children = [#xmlcdata{content = <<"error">>}]}
    end.

conversation_element(E, C) ->
    #xmlel{name = atom_to_binary(E, utf8),
           children = [conversation_element_data(E, maps:get(E, C))]}.

conversation_element_data(outgoing, V) ->
    #xmlcdata{content = atom_to_binary(V, utf8)};
conversation_element_data(timestamp, V) ->
    #xmlcdata{content = integer_to_binary(V div 1000)};
conversation_element_data(_, V) ->
    #xmlcdata{content = V}.
