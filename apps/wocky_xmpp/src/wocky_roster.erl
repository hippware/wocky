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
                  contact := Contact,
                  name := Name,
                  ask := Ask,
                  subscription := Subscription,
                  groups := Groups
                 }) ->
    ContactJID = jid:to_lower(jid:make(ContactID, wocky_app:server(), <<>>)),
    #wocky_roster{
       user = UserID,
       server = wocky_app:server(),
       contact_jid = ContactJID,
       name = Name,
       subscription = Subscription,
       ask = Ask,
       groups = Groups,
       avatar = safe_value(maps:get(avatar, Contact, nil)),
       contact_handle = safe_value(maps:get(handle, Contact, nil)),
       first_name = safe_value(maps:get(first_name, Contact, nil)),
       last_name = safe_value(maps:get(last_name, Contact, nil))}.

safe_value(nil) -> <<>>;
safe_value(Value) -> Value.
