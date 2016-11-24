%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Utility module for wocky bots. Contains functions used by
%%% multiple bot implementation modules
%%%
-module(wocky_bot_util).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").
-include("wocky.hrl").
-include("wocky_bot.hrl").

-compile({parse_transform, cut}).
-compile({parse_transform, fun_chain}).

-export([check_owner/3,
         check_access/3,
         check_bot_exists/2,
         get_id_from_node/1,
         get_id_from_jid/1,
         notify_affiliates/3,
         make_affiliate_elements/1,
         make_jid/2,
         make_node/1,
         list_hash/1,
         get_image/1
        ]).

check_owner(Server, ID, User) ->
    case jid:are_bare_equal(wocky_db_bot:owner(Server, ID), User) of
        true -> ok;
        false -> {error, ?ERR_FORBIDDEN}
    end.

check_access(Server, ID, From) ->
    case wocky_db_bot:has_access(Server, ID, From) of
        true -> ok;
        false -> {error, ?ERR_FORBIDDEN};
        not_found -> {error, ?ERR_ITEM_NOT_FOUND}
    end.

check_bot_exists(Server, ID) ->
    case wocky_db_bot:exists(Server, ID) of
        true -> ok;
        false -> {error, ?ERR_ITEM_NOT_FOUND}
    end.

get_id_from_node(Attrs) ->
    case wocky_xml:get_attr(<<"node">>, Attrs) of
        {error, E} -> {error, E};
        {ok, NodeValue} -> get_id_from_node_value(NodeValue)
    end.

get_id_from_node_value(Node) ->
    case binary:split(Node, <<$/>>, [global]) of
        [<<"bot">>, ID] -> {ok, ID};
        _ -> {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Invalid bot node">>)}
    end.

get_id_from_jid(#jid{lresource = <<"bot/", ID/binary>>}) ->
    ID;
get_id_from_jid(_) ->
    <<>>.

notify_affiliates(Sender, ID, Affiliates) ->
    lists:foreach(notify_affiliate(Sender, ID, _), Affiliates).

notify_affiliate(Sender, ID, {User, Role}) ->
    ejabberd_router:route(Sender, User, make_update_packet(ID, User, Role)).

make_update_packet(ID, User, Role) ->
    AffiliateEl = make_affiliate_element({User, Role}),
    #xmlel{name = <<"message">>,
           children = [#xmlel{name = <<"affiliations">>,
                              attrs = [{<<"xmlns">>, ?NS_BOT},
                                       {<<"node">>, make_node(ID)}],
                              children = [AffiliateEl]}]}.

make_affiliate_elements(Affiliates) ->
    lists:map(fun make_affiliate_element/1, Affiliates).

make_affiliate_element({JID, Affiliation}) ->
    #xmlel{name = <<"affiliation">>,
           attrs = [{<<"jid">>, jid:to_binary(JID)},
                    {<<"affiliation">>, atom_to_binary(Affiliation, utf8)}]}.

list_hash(List) ->
    fun_chain:first(
      List,
      lists:sort(),
      term_to_binary(),
      erlang_murmurhash:murmurhash3_x64_128(),
      integer_to_binary(36)
     ).

make_jid(Server, ID) ->
    jid:make(<<>>, Server, make_node(ID)).

make_node(ID) ->
    <<"bot/", ID/binary>>.

get_image(Entry) when is_binary(Entry) ->
    case exml:parse(Entry) of
        {ok, EntryXML} -> get_image(EntryXML);
        _ -> none
    end;
get_image(Entry = #xmlel{}) ->
    case xml:get_subtag(Entry, <<"image">>) of
        #xmlel{children = [#xmlcdata{content = C}]} -> C;
        false -> none
    end.
