%%% @copyright 2016+ Hippware, Inc.
-ifndef(WOCKY_REG_HRL).
-define(WOCKY_REG_HRL, 1).

-record(reg_result, {
          user          :: ejabberd:luser(),
          server        :: ejabberd:lserver(),
          provider      :: binary(),
          is_new        :: boolean(),
          token         :: 'Elixir.Wocky.User.Token':t() | undefined,
          token_expiry  :: binary(),
          external_id   :: binary()
         }).

-endif. % WOCKY_REG_HRL
