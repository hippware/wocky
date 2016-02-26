%%% @copyright 2016+ Hippware, Inc.
%%% @doc Wocky utility functions
%%%
%%% This is a module for wocky utility functions that don't obviously warrant
%%% their own module but are used from multiple places.
-module(wocky_util).

-export([
   add_hooks/4,
   delete_hooks/4,
   ssl_opts/0
        ]).

-export_type([hook/0]).

-type hook() :: {Hook :: atom(), Callback :: atom()}.

%% @doc Register a set of hooks with ejabberd's hook system
-spec add_hooks(
        Hooks :: [hook()],
        Host  :: binary(),
        Module :: atom(),
        Sequence :: non_neg_integer()) -> ok.
add_hooks(Hooks, Host, Module, Sequence) ->
    lists:foreach(
      fun ({Hook, Callback}) ->
              ejabberd_hooks:add(Hook, Host, Module, Callback, Sequence)
      end, Hooks).

%% @doc Deregister a set of hooks with ejabberd's hook system
-spec delete_hooks(
        Hooks :: [hook()],
        Host  :: binary(),
        Module :: atom(),
        Sequence :: non_neg_integer()) -> ok.
delete_hooks(Hooks, Host, Module, Sequence) ->
    lists:foreach(
      fun ({Hook, Callback}) ->
              ejabberd_hooks:delete(Hook, Host, Module, Callback, Sequence)
      end, Hooks).

%% @doc Returns key/cert config items for webmachine or cowboy
ssl_opts() ->
    [
     {cacertfile, ssl_file("fake_cert.pem")},
     {certfile, ssl_file("fake_server.pem")},
     {keyfile, ssl_file("fake_key.pem")}
    ].

ssl_file(Filename) ->
    PrivDir = code:priv_dir(wocky),
    filename:join([PrivDir, "ssl", Filename]).

