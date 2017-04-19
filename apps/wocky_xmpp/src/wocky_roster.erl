-module(wocky_roster).

-include_lib("ejabberd/include/jlib.hrl").
-include("wocky_roster.hrl").

-export([to_wocky_roster/3, to_wocky_roster/1]).

to_wocky_roster(LUser, ContactBJID, nil) when is_binary(ContactBJID) ->
    ContactJID = jid:to_lower(jid:from_binary(ContactBJID)),
    to_wocky_roster(LUser, ContactJID, nil);
to_wocky_roster(LUser, ContactJID = {_, _, _}, nil) ->
    #wocky_roster{
       user = LUser,
       server = wocky_app:server(),
       contact_jid = ContactJID};
to_wocky_roster(_LUser, _ContactJID, RosterItem) when is_map(RosterItem) ->
    to_wocky_roster(RosterItem).

to_wocky_roster(#{user_id := UserID,
                  contact_id := ContactID,
                  name := Name,
                  ask := Ask,
                  subscription := Subscription,
                  groups := Groups
                 }) ->
    ContactJID = jid:to_lower(jid:make(ContactID, wocky_app:server(), <<>>)),
    R = #wocky_roster{
           user = UserID,
           server = wocky_app:server(),
           contact_jid = ContactJID,
           name = Name,
           subscription = Subscription,
           ask = Ask,
           groups = Groups},
    fill_extra_fields(R).

fill_extra_fields(R = #wocky_roster{contact_jid = {UserID, _, _}}) ->
    case ?wocky_user:find(UserID) of
        nil ->
            R;

        User ->
            R#wocky_roster{
              avatar = safe_value(maps:get(avatar, User, nil)),
              contact_handle = safe_value(maps:get(handle, User, nil)),
              first_name = safe_value(maps:get(first_name, User, nil)),
              last_name = safe_value(maps:get(last_name, User, nil))
             }
    end.

safe_value(nil) -> <<>>;
safe_value(Value) -> Value.
