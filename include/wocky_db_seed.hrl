%%% @copyright 2016+ Hippware, Inc.
%%% @doc Macros useful for seed data used in tests

-ifndef(WOCKY_DB_SEED_HRL).
-define(WOCKY_DB_SEED_HRL, 1).

-define(LOCAL_CONTEXT, <<"localhost">>).

-define(USER,    <<"043e8c96-ba30-11e5-9912-ba0be0483c18">>).
-define(SERVER,  <<"localhost">>).
-define(RESOURCE,<<"testing">>).
-define(HANDLE,  <<"alice">>).
-define(PHONE_NUMBER,<<"+1234">>).
-define(PASS,    <<"password">>).
-define(SCRAM,   <<"==SCRAM==,8dncGDJ45eXoMzqZ3zT6F/8gJt0=,"
                   "aEsAiUKWIdvXDW5oSaQxUJKHcOU=,Q0t9V/6+rkA1jv0LGA/IHw==,"
                   "4096">>).
-define(TOKEN,   <<"$T$QU15BwZioTHgSlcIk6D0odmfaXirL9ajzu7qeeFkAuU=">>).

-define(BADUSER, <<"d51f92c8-ba40-11e5-9912-ba0be0483c18">>).
-define(NEWUSER, <<"31a07b20-c5fe-11e5-9912-ba0be0483c18">>).

-define(ALICE,  <<"043e8c96-ba30-11e5-9912-ba0be0483c18">>).
-define(CAROL,  <<"1e6f3b36-c5fe-11e5-9912-ba0be0483c18">>).
-define(BOB,    <<"2396aa72-c5fe-11e5-9912-ba0be0483c18">>).
-define(KAREN,  <<"2841ef1e-c5fe-11e5-9912-ba0be0483c18">>).
-define(ROBERT, <<"2c33f32e-c5fe-11e5-9912-ba0be0483c18">>).
-define(TIM,    <<"31a07b20-c5fe-11e5-9912-ba0be0483c18">>).

-endif.
