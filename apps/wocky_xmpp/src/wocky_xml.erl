-module(wocky_xml).

-compile({parse_transform, cut}).

-include("wocky.hrl").
-include_lib("exml/include/exml_stream.hrl").

-export([
         get_subel_cdata/3,
         get_subel_cdata/2,
         act_on_subel_cdata/3,
         get_subel/2,
         act_on_subel/3,
         foldl_subels/3,
         check_namespace/2,
         check_attr/3,
         get_attr/2,
         parse_multiple/1,
         cdata_el/2,
         path_by_attr/4
        ]).

-type error() :: {error, jlib:xmlel()}.

-spec get_subel_cdata(binary(), jlib:xmlel(), binary()) -> {ok, binary()}.
get_subel_cdata(TagName, Element, Default) ->
    case get_subel_cdata(TagName, Element) of
        R = {ok, _} -> R;
        {error, _} -> {ok, Default}
    end.

-spec get_subel_cdata(binary(), jlib:xmlel()) -> {ok, binary()} | error().
get_subel_cdata(TagName, Element) ->
    act_on_subel_cdata(TagName, Element, id_pass_error(_)).

-spec act_on_subel_cdata(binary(), jlib:xmlel(), fun((binary()) -> X))
    -> X | error().
act_on_subel_cdata(TagName, Element, Fun) ->
    Fun(act_on_subel(TagName, Element, fun xml:get_tag_cdata/1)).

-spec get_subel(binary(), jlib:xmlel()) -> {ok, jlib:xmlel()} | error().
get_subel(TagName, Element) ->
    act_on_subel(TagName, Element, id_pass_error(_)).

-spec act_on_subel(binary(), jlib:xmlel(), fun((jlib:xmlel()) -> X))
    -> X | error().
act_on_subel(TagName, Element, Fun) ->
    case xml:get_subtag(Element, TagName) of
        TagEl = #xmlel{} ->
            Fun(TagEl);
        false ->
            {error,
             ?ERRT_BAD_REQUEST(?MYLANG,
                               <<"<", TagName/binary, "> element required">>)}
    end.

-spec foldl_subels(jlib:xmlel(), A, fun((jlib:xmlel(), A) -> {ok, A} | error()))
-> {ok, A} | error().
foldl_subels(#xmlel{children = Children}, Acc, Fun) ->
    foldl_subels1(Fun, Acc, Children).

foldl_subels1(_Fun, Acc, []) ->
    {ok, lists:reverse(Acc)};
foldl_subels1(Fun, Acc, [H|T]) ->
    case Fun(H, Acc) of
        {error, E} -> {error, E};
        {ok, Acc2} -> foldl_subels1(Fun, Acc2, T)
    end.

-spec check_namespace(binary(), jlib:xmlel()) -> {ok, jlib:xmlel()} | error().
check_namespace(NS, El = #xmlel{name = Name, attrs = Attrs}) ->
    case xml:get_attr_s(<<"xmlns">>, Attrs) of
        NS ->
            {ok, El};
        <<>> ->
            {error, ?ERRT_BAD_REQUEST(?MYLANG,
                                      <<"Missing namespace on element ",
                                        Name/binary>>)};
        X ->
            {error, ?ERRT_BAD_REQUEST(?MYLANG,
                                      <<"Invlid namespace: ", X/binary>>)}
    end.

check_attr(Attr, Value, #xmlel{attrs = Attrs}) ->
    case xml:get_attr(Attr, Attrs) of
        {value, Value} -> ok;
        _ ->
            {error, ?ERRT_BAD_REQUEST(?MYLANG,
                                      <<"Invalid or missing attribute: ",
                                        Attr/binary>>)}
    end.

-spec get_attr(binary(), jlib:xmlel() | [exml:attr()])
-> {ok, binary()} | error().

get_attr(AttrName, #xmlel{attrs = Attrs}) ->
    get_attr(AttrName, Attrs);
get_attr(AttrName, Attrs) when is_list(Attrs) ->
    case xml:get_attr(AttrName, Attrs) of
        {value, Val} ->
            {ok, Val};
        false ->
            {error, ?ERRT_BAD_REQUEST(?MYLANG,
                                      <<"Missing ", AttrName/binary,
                                        " attribute">>)}
    end.

%% Same as exml:parse except it can handle cases with multiple top-level
%% elements, eg <<"<a/><b/>">>.
-spec parse_multiple(binary()) -> {ok, [jlib:xmlel()]} | {error, any()}.
parse_multiple(XML) ->
    {ok, Parser} = exml_stream:new_parser(),
    Stream = <<"<stream>", XML/binary, "</stream>">>,
    Result = case exml_stream:parse(Parser, Stream) of
                 {ok, _, [#xmlstreamstart{} | Tree]} ->
                     get_tree(Tree);
                 {ok, _, Other} ->
                     {error, {bad_parse, Other}};
                 {error, Error} ->
                     {error, Error}
             end,
    ok = exml_stream:free_parser(Parser),
    Result.

get_tree(Tree) ->
    case lists:last(Tree) of
        #xmlstreamend{} ->
            {ok, lists:droplast(Tree)};
        _ ->
            {error, {bad_parse, Tree}}
    end.

id_pass_error({error, E}) -> {error, E};
id_pass_error(X)          -> {ok, X}.

cdata_el(Name, Value) ->
    #xmlel{name = Name, children = [#xmlcdata{content = Value}]}.

% Find the first element at Path (see exml_query.erl) with the specified
% attribute pair
-spec path_by_attr(jlib:xmlel(), exml_query:path(), binary(), binary()) ->
    jlib:xmlel() | undefined.
path_by_attr(Element, Path, AttrName, AttrValue) ->
    Elements = exml_query:paths(Element, Path),
    L = lists:dropwhile(
          fun(E) -> exml_query:attr(E, AttrName) =/= AttrValue end,
          Elements),
    case L of
        [] -> undefined;
        [E|_] -> E
    end.
