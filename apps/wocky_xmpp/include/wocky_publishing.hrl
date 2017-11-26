%%% @copyright 2016+ Hippware, Inc.
-ifndef(WOCKY_PUBLISHING_HRL).
-define(WOCKY_PUBLISHING_HRL, 1).

-include("wocky.hrl").

-type pub_version() :: binary() | undefined.
-type pub_ordering() :: binary() | undefined.
-type pub_extra_data() :: [jlib:xmlel()].
-type pub_item_id() :: binary().
-type published_stanza() :: jlib:xmlel() | [jlib:xmlel()].

-record(published_item, {
          id                :: pub_item_id(),
          type = <<"item">> :: binary(),
          new = false       :: boolean(),
          version           :: pub_version(),
          from              :: ejabberd:jid(),
          stanza            :: published_stanza(),
          deleted = false   :: boolean()
         }).

-type pub_item() :: #published_item{}.

-type pub_node() :: ejabberd:lresource().

-type pub_presence_type() :: available | unavailable.

-type pub_error_result() :: {error, jlib:xmlel()}.

-type pub_result() :: ok | pub_error_result().

-type pub_get_result() ::
    {ok, {[pub_item()], pub_version(), pub_extra_data(), jlib:rsm_out()} |
         {pub_item(), pub_version(), pub_extra_data()} |
         not_found} |
    pub_error_result().


-define(PUBLISHING_HANDLER_TABLE, mod_wocky_publishing_handlers).

-endif. % ifdef WOCKY_PUBLISHING_HRL
