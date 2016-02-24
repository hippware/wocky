-module(user_default).

-export([help/0, binpp/1, ppst/0, loglevel/1, dbgtc/1, dbgon/1, dbgon/2,
         dbgadd/1, dbgadd/2, dbgdel/1, dbgdel/2, dbgoff/0]).

-import(io, [format/1]).

help() ->
    shell_default:help(),
    format("** user extended commands **~n"),

    format("binpp(Binary) -- Pretty print Binary using binpp\n"),
    format("ppst()        -- Pretty print the last stack trace\n"),
    format("loglevel(Lvl) -- Change the log level for the console\n"),

    format("dbgtc(File)   -- use dbg:trace_client() to read data from File\n"),
    format("dbgon(M)      -- enable dbg tracer on all funs in module M\n"),
    format("dbgon(M,Fun)  -- enable dbg tracer for module M and function F\n"),
    format("dbgon(M,File) -- enable dbg tracer for module M and log to File\n"),
    format("dbgadd(M)     -- enable call tracer for module M\n"),
    format("dbgadd(M,F)   -- enable call tracer for function M:F\n"),
    format("dbgdel(M)     -- disable call tracer for module M\n"),
    format("dbgdel(M,F)   -- disable call tracer for function M:F\n"),
    format("dbgoff()      -- disable dbg tracer (calls dbg:stop/0)\n"),
    true.

binpp(Bin) ->
    binpp:pprint(Bin).

ppst() ->
    pe:st(erlang:get_stacktrace()).

loglevel(Lvl) ->
    lager:set_loglevel(lager_console_backend, Lvl).

dbgtc(File) ->
    Fun = fun({trace,_,call,{M,F,A}}, _) ->
                 io:format("call: ~w:~w~w~n", [M,F,A]);
             ({trace,_,return_from,{M,F,A},R}, _) ->
                 io:format("retn: ~w:~w/~w -> ~w~n", [M,F,A,R]);
             (A,B) ->
                 io:format("~w: ~w~n", [A,B])
          end,
    dbg:trace_client(file, File, {Fun, []}).

dbgon(Module) ->
    case dbg:tracer() of
        {ok,_} ->
            dbg:p(all,call),
            dbg:tpl(Module, [{'_',[],[{return_trace}]}]),
            ok;
        Else ->
            Else
    end.

dbgon(Module, Fun) when is_atom(Fun) ->
    {ok,_} = dbg:tracer(),
    dbg:p(all,call),
    dbg:tpl(Module, Fun, [{'_',[],[{return_trace},{exception_trace}]}]),
    ok;

dbgon(Module, File) when is_list(File) ->
    {ok,_} = dbg:tracer(port, dbg:trace_port(file, File)),
    dbg:p(all,[call, running, garbage_collection, timestamp, return_to]),
    dbg:tpl(Module, [{'_',[],[{return_trace},{exception_trace}]}]),
    ok;

dbgon(Module, TcpPort) when is_integer(TcpPort) ->
    io:format("Use this command on the node you're tracing (-remsh ...)\n"),
    io:format("Use dbg:stop() on target node when done.\n"),
    {ok,_} = dbg:tracer(port, dbg:trace_port(ip, TcpPort)),
    dbg:p(all,call),
    dbg:tpl(Module, [{'_',[],[{return_trace},{exception_trace}]}]),
    ok.

dbgadd(Module) ->
    dbg:tpl(Module, [{'_',[],[{return_trace},{exception_trace}]}]),
    ok.

dbgadd(Module, Fun) ->
    dbg:tpl(Module, Fun, [{'_',[],[{return_trace},{exception_trace}]}]),
    ok.

dbgdel(Module) ->
    dbg:ctpl(Module),
    ok.

dbgdel(Module, Fun) ->
    dbg:ctpl(Module, Fun),
    ok.

dbgoff() ->
    dbg:stop().
