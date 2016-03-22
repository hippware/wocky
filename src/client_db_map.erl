-module(client_db_map).

-export([fields/0,
         client_to_db/1,
         db_to_client/1]).

fields() ->
      %JSON/IQ Tag  %DB field name
    [{userID,       auth_user},
     {uuid,         user},
     {server,       server},
     {handle,       handle},
     {firstName,    first_name},
     {lastName,     last_name},
     {phoneNumber,  phone_number},
     {email,        email},
     {avatar,       avatar}
    ].

-spec client_to_db(atom()) -> atom().
client_to_db(ClientField) ->
    proplists:get_value(ClientField, fields()).

-spec db_to_client(atom()) -> atom().
db_to_client(DBField) ->
    case lists:keyfind(DBField, 2, fields()) of
        false -> undefined;
        {ClientField, _} -> ClientField
    end.
