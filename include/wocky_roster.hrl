%%% @copyright 2016+ Hippware, Inc.
-ifndef(WOCKY_ROSTER_HRL).
-define(WOCKY_ROSTER_HRL, 1).

-record(roster, {
          user                  :: binary(),
          server                :: binary(),
          contact_jid           :: ejabberd:simple_jid(),
          contact_handle = <<>> :: binary(),
          first_name = <<>>     :: binary(),
          last_name = <<>>      :: binary(),
          name = <<>>           :: binary(),
          avatar = <<>>         :: binary(),
          subscription = none   :: both | from | to | none | remove,
          ask = none            :: in | out | both | none,
          groups = []           :: [binary()],
          ask_message = <<>>    :: binary(),
          xs = []               :: [term()]
         }).

-endif. % ifdef WOCKY_ROSTER_HRL
