%%% @copyright 2016+ Hippware, Inc.
-ifndef(MOD_HXEP_FRANCUS_HRL).
-define(MOD_HXEP_FRANCUS_HRL, 1).

-record(hxep_request, {
          op            :: get | put,
          request       :: {binary(), binary(), binary()}, % {User, File, Auth}
          user_server   :: binary(),
          size          :: integer(),
          metadata      :: francus:metadata(),
          tref          :: timer:tref()
         }).

-endif. % ifdef MOD_HXEP_FRANCUS_HRL
