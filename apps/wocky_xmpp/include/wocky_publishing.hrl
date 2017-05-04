%%% @copyright 2016+ Hippware, Inc.
-ifndef(WOCKY_PUBLISHING_HRL).
-define(WOCKY_PUBLISHING_HRL, 1).

-include("wocky.hrl").

-type pub_version() :: ?wocky_id:t() | not_found.
-type pub_item_id() :: binary().
-type published_stanza() :: jlib:xmlel() | [jlib:xmlel()].

-record(published_item, {
          id        :: pub_item_id(),
          version   :: pub_version(),
          from      :: ejabberd:jid(),
          stanza    :: published_stanza(),
          deleted = false :: boolean()
         }).

-type published_item() :: #published_item{}.
-type publishing_node() :: binary().

-type pub_presence_type() :: available | unavailable.

-define(PUBLISHING_HANDLER_TABLE, mod_wocky_publishing_handlers).

-endif. % ifdef WOCKY_PUBLISHING_HRL
