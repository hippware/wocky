%%% @copyright 2016+ Hippware, Inc.
-ifndef(WOCKY_REG_HRL).
-define(WOCKY_REG_HRL, 1).

-include("wocky.hrl").

-record(reg_result, {
          user          :: ejabberd:luser(),
          server        :: ejabberd:lserver(),
          provider      :: binary(),
          is_new        :: boolean(),
          token         :: ?wocky_account:token() | undefined,
          token_expiry  :: binary(),
          external_id   :: binary()
         }).

-endif. % WOCKY_REG_HRL
