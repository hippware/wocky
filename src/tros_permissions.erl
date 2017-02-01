-module(tros_permissions).

-compile({parse_transform, cut}).
-compile({parse_transform, fun_chain}).

-export([
         can_download/3,
         access_rules_from_list/1
        ]).

-include_lib("ejabberd/include/jlib.hrl").

can_download(User = #jid{luser = UserID}, OwnerID, Access) ->
    case OwnerID of
        UserID ->
            true; %% Users can always get their own files
        _ ->
            can_download_access(User, Access)
    end.

can_download_access(User, Access) ->
    Result = fun_chain:last(
               Access,
               access_rules_from_list(),
               matches_any_rule(User)),
    case Result of
        true -> true;
        false -> {false, permission_denied}
    end.

access_rules_from_list(Access) ->
    List = split_access_list(Access),
    lists:filter(fun(R) -> R =/= invalid end,
                 lists:map(to_rule(_), List)).

split_access_list(Access) ->
    binary:split(Access, <<$,>>, [global, trim_all]).

to_rule(<<"all">>) -> all;
to_rule(<<"user:", User/binary>>) -> {user, jid:from_binary(User)};
to_rule(<<"friends:", User/binary>>) -> {friends, jid:from_binary(User)};
to_rule(<<"members:", Group/binary>>) -> {members, jid:from_binary(Group)};
to_rule(<<"redirect:", Target/binary>>) -> {redirect, jid:from_binary(Target)};
to_rule(P) ->
    ok = lager:info("Invalid permission: ~p", [P]),
    invalid.

matches_any_rule(User, Rules) ->
    SortedRules = lists:usort(rule_sort(_, _), Rules),
    lists:any(matches_rule(User, _), SortedRules).

% Sort the fastest to evalue rules first and the slowest last
rule_sort({Type, V1}, {Type, V2}) -> V1 =< V2;
rule_sort(all, all) -> true;
rule_sort(all, _) -> true;
rule_sort(_, all) -> false;
rule_sort({user, _}, _) -> true;
rule_sort({friends, _}, {Type, _}) -> Type =/= user;
rule_sort({members, _}, {Type, _}) -> Type =/= user andalso Type =/= friends;
rule_sort({redirect, _}, _) -> false.

matches_rule(_, all) -> true;
matches_rule(User, {user, RuleUser}) ->
    jid:are_bare_equal(User, RuleUser);
matches_rule(User, {friends, RuleUser}) ->
    wocky_db_roster:is_friend(RuleUser, User);
matches_rule(User, {members, Group}) ->
    mod_wocky_group_chat:is_participant(User, Group);
matches_rule(User, {redirect, Target}) ->
    access_query:run(Target, User, view) =:= allow.
