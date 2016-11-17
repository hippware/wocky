-module(wocky_xml).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("exml/include/exml_stream.hrl").

-compile({parse_transform, cut}).

-export([
         get_subel_cdata/2,
         act_on_subel_cdata/3,
         get_subel/2,
         act_on_subel/3,
         check_namespace/2,
         check_attr/3,
         get_attr/2,
         get_sub_el/2,
         parse_multiple/1
        ]).

-ignore_xref([check_attr/3]).

-type error() :: {error, jlib:xmlel()}.

-spec get_subel_cdata(binary(), jlib:xmlel()) -> {ok, binary()} | error().
get_subel_cdata(TagName, Element) ->
    act_on_subel_cdata(TagName, Element, id_pass_error(_)).

-spec act_on_subel_cdata(binary(), jlib:xmlel(), fun((binary()) -> X))
    -> X | error().
act_on_subel_cdata(TagName, Element, Fun) ->
    Fun(act_on_subel(TagName, Element, fun xml:get_tag_cdata/1)).

-spec get_subel(binary(), jlib:xmlel()) -> {ok, binary()} | error().
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

-spec get_attr(binary(), [exml:attr()]) -> {ok, binary()} | error().
get_attr(AttrName, Attrs) ->
    case xml:get_attr(AttrName, Attrs) of
        {value, Val} ->
            {ok, Val};
        false ->
            {error, ?ERRT_BAD_REQUEST(?MYLANG,
                                      <<"Missing ", AttrName/binary,
                                        " attribute">>)}
    end.

-spec get_sub_el(binary(), jlib:xmlel()) ->
    {error, jlib:xmlel()} | {ok, jlib:xmlel()}.
get_sub_el(Name, El) ->
    case xml:get_path_s(El, [{elem, Name}]) of
        <<>> -> {error, ?ERRT_BAD_REQUEST(?MYLANG, <<"Missing '", Name/binary,
                                                     "' element">>)};
        SubEl -> {ok, SubEl}
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
