%%% @copyright 2016+ Hippware, Inc.
-ifndef(WOCKY_ROSTER_HRL).
-define(WOCKY_ROSTER_HRL, 1).

-type subscription_type() :: both | from | to | none | remove.
-type ask_type() :: in | out | both | none.

-record(wocky_roster, {
          user                  :: binary(),
          server                :: binary(),
          contact_jid           :: ejabberd:simple_jid(),
          contact_handle = <<>> :: binary(),
          first_name = <<>>     :: binary(),
          last_name = <<>>      :: binary(),
          name = <<>>           :: binary(),
          avatar = <<>>         :: binary(),
          subscription = none   :: subscription_type(),
          ask = none            :: ask_type(),
          groups = []           :: [binary()],
          xs = []               :: [term()]
         }).

-type wocky_roster() :: #wocky_roster{}.

-endif. % ifdef WOCKY_ROSTER_HRL
