%%% @copyright 2016+ Hippware, Inc.
-ifndef(MOD_TROS_FRANCUS_HRL).
-define(MOD_TROS_FRANCUS_HRL, 1).

-record(tros_request, {
          op            :: get | put,
          request       :: {binary(), binary(), binary()}, % {User, File, Auth}
          user_server   :: binary(),
          size          :: integer(),
          metadata      :: francus:metadata(),
          tref          :: timer:tref()
         }).

-endif. % ifdef MOD_TROS_FRANCUS_HRL
