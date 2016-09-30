%%% @copyright 2016+ Hippware, Inc.
%%% @doc Behaviour definition for mod_wocky_tros backends
-module(mod_wocky_tros_backend).

%% I die a little inside when I have to do this.
-ignore_xref([behaviour_info/1]).

-callback start(list()) -> any().

-callback stop() -> any().

-callback get_owner(ejabberd:lserver(), binary()) ->
    {ok, any()} | {error, any()}.

-callback get_metadata(ejabberd:lserver(), binary()) ->
    {ok, any()} | {error, any()}.

-callback make_upload_response(ejabberd:jid(),
                               ejabberd:jid(),
                               binary(),
                               integer(),
                               binary(),
                               map()) ->
    {list(), list()}.

-callback make_download_response(ejabberd:jid(),
                                 ejabberd:jid(),
                                 binary(),
                                 binary(),
                                 map()) ->
    {list(), list()}.
