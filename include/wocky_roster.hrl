%%% @copyright 2016+ Hippware, Inc.
-ifndef(WOCKY_ROSTER_HRL).
-define(WOCKY_ROSTER_HRL, 1).

-record(roster, {
          user                :: binary(),
          server              :: binary(),
          contact_jid         :: ejabberd:simple_jid(),
          name = <<>>         :: binary(),
          subscription = none :: both | from | to | none | remove,
          ask = none          :: in | out | both | none,
          groups = []         :: [binary()],
          askmessage = <<>>   :: binary(),
          xs = []             :: [term()]
         }).

-endif. % ifdef WOCKY_ROSTER_HRL
