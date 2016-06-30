%%% @copyright 2016+ Hippware, Inc.
-ifndef(MOD_TROS_FRANCUS_HRL).
-define(MOD_TROS_FRANCUS_HRL, 1).

-record(tros_request, {
          method        :: get | post,
          user          :: binary(),
          file          :: binary(),
          auth          :: binary(),
          size          :: integer(),
          purpose       :: binary(),
          access        :: binary(),
          metadata      :: francus:metadata()
         }).

-endif. % ifdef MOD_TROS_FRANCUS_HRL
