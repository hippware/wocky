-module(wocky_waiter).

-compile({parse_transform, cut}).

-export([wait/3, notify/1]).

wait(Event, Timeout, SkipCallback) ->
    Ref = make_ref(),
    Key = key(Event),
    Val = val(self(), Ref),
    ejabberd_redis:cmd(["SADD", Key, Val]),
    Result = case SkipCallback() of
                 true -> ok;
                 false -> do_wait(Ref, Timeout)
             end,
    ejabberd_redis:cmd(["SREM", Key, Val]),
    Result.

do_wait(Ref, Timeout) ->
    receive
        {waiter_event, Ref} -> ok
        after Timeout -> timeout
    end.

notify(Event) ->
    Vals = ejabberd_redis:cmd(["SMEMBERS", key(Event)]),
    lists:foreach(notify_waiter(_), Vals).

notify_waiter(Waiter) ->
    {Pid, Ref} = binary_to_term(Waiter),
    Pid ! {waiter_event, Ref}.

key(Event) -> ["wait:", Event].

val(Pid, Ref) -> term_to_binary({Pid, Ref}).
