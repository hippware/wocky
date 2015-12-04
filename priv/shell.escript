#!/usr/bin/env escript
%%! -noshell -noinput
%% -*- mode: erlang;erlang-indent-level: 2;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=2 sw=2 et

main([]) ->
    ok = filelib:ensure_dir("_build/default/log/sasl/dummy").
