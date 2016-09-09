-module(wocky_xml).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").

-export([
         get_subel_cdata/2,
         act_on_subel_cdata/3,
         act_on_subel/3,
         check_namespace/2,
         get_attr/2
        ]).

-spec get_subel_cdata(binary(), exml:element()) -> {ok, binary()} |
                                                   {error, exml:element()}.
get_subel_cdata(TagName, Element) ->
    act_on_subel_cdata(TagName, Element, fun({error, E}) -> {error, E};
                                            (X)          -> {ok, X}
                                         end).

-spec act_on_subel_cdata(binary(), exml:element(), fun((binary()) -> X))
    -> X | {error, exml:element()}.
act_on_subel_cdata(TagName, Element, Fun) ->
    Fun(act_on_subel(TagName, Element, fun xml:get_tag_cdata/1)).

-spec act_on_subel(binary(), exml:element(), fun((exml:element()) -> X))
    -> X | {error, exml:element()}.
act_on_subel(TagName, Element, Fun) ->
    case xml:get_subtag(Element, TagName) of
        TagEl = #xmlel{} ->
            Fun(TagEl);
        false ->
            {error,
             ?ERRT_BAD_REQUEST(?MYLANG,
                               <<"<", TagName/binary, "> element required">>)}
    end.

check_namespace(NS, El = #xmlel{name = Name, attrs = Attrs}) ->
    case xml:get_attr_s(<<"xmlns">>, Attrs) of
        NS ->
            {ok, El};
        <<>> ->
            {error, ?ERRT_BAD_REQUEST(?MYLANG,
                                      <<"Missing namespace on element",
                                        Name/binary>>)};
        X ->
            {error, ?ERRT_BAD_REQUEST(?MYLANG,
                                      <<"Invlid namespace: ", X/binary>>)}
    end.

get_attr(AttrName, Attrs) ->
    case xml:get_attr(AttrName, Attrs) of
        {value, Val} ->
            {ok, Val};
        false ->
            {error, ?ERRT_BAD_REQUEST(?MYLANG,
                                      <<"Missing ", AttrName/binary,
                                        " attribute">>)}
    end.
