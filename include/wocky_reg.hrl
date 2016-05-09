%%% @copyright 2016+ Hippware, Inc.
-ifndef(WOCKY_REG_HRL).
-define(WOCKY_REG_HRL, 1).

-record(reg_result, {
          user          :: ejabberd:luser(),
          server        :: ejabberd:lserver(),
          provider      :: binary(),
          is_new        :: boolean(),
          token         :: wocky_db_user:token() | undefined,
          token_expiry  :: non_neg_integer(),
          external_id   :: binary()
         }).

-endif. % WOCKY_REG_HRL
