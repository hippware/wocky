%%% @copyright 2016+ Hippware, Inc.
-ifndef(WOCKY_ROSTER_HRL).
-define(WOCKY_ROSTER_HRL, 1).

-include("wocky.hrl").

-record(wocky_roster, {
          user                  :: binary(),
          server                :: binary(),
          contact_jid           :: ejabberd:simple_jid(),
          contact_handle = <<>> :: binary() | nil,
          first_name = <<>>     :: binary() | nil,
          last_name = <<>>      :: binary() | nil,
          name = <<>>           :: binary(),
          avatar = <<>>         :: binary() | nil,
          subscription = none   :: ?wocky_roster_item:subscription(),
          ask = none            :: ?wocky_roster_item:ask(),
          created_at            :: ?datetime:t(),
          groups = []           :: [binary()],
          xs = []               :: [term()]
         }).

-type wocky_roster() :: #wocky_roster{}.

-endif. % ifdef WOCKY_ROSTER_HRL
