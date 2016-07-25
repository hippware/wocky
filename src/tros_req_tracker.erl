%%% @copyright 2016+ Hippware, Inc.
%%% @doc TROS request tracker for Francus interface
-module(tros_req_tracker).

-export([
         start/0,
         stop/0,
         add/1,
         check/4
        ]).

-include("mod_tros_francus.hrl").

start() -> ok.

stop() -> ok.

add(#tros_request{
       user     = User,
       file     = File,
       auth     = Auth,
       method   = Method,
       size     = Size,
       metadata = Metadata,
       purpose  = Purpose,
       access   = Access}) ->

    Q = "INSERT INTO tros_request "
        "(user, file, auth, method, size, metadata, purpose, access)"
        " VALUES(?, ?, ?, ?, ?, ?, ?, ?) USING TTL ?",
    V = #{user     => User,
          file     => File,
          auth     => Auth,
          method   => atom_to_binary(Method, utf8),
          size     => Size,
          metadata => Metadata,
          purpose  => Purpose,
          access   => Access,
          '[ttl]'  => timeout()},
    {ok, void} = wocky_db:query(wocky_app:server(), Q, V, quorum),
    ok.

check(User, File, Auth, Method) ->
    R = wocky_db:select_row(wocky_app:server(), tros_request, all,
                            #{user   => User,
                              file   => File,
                              auth   => Auth,
                              method => atom_to_binary(Method, utf8)}),
    case R of
        not_found -> false;
        _ -> row_to_rec(R)
    end.

row_to_rec(#{
  user     := User,
  file     := File,
  auth     := Auth,
  method   := Method,
  size     := Size,
  metadata := Metadata,
  purpose  := Purpose,
  access   := Access
 }) ->

    #tros_request{user     = User,
                  file     = File,
                  auth     = Auth,
                  method   = binary_to_existing_atom(Method, utf8),
                  size     = Size,
                  metadata = Metadata,
                  purpose  = Purpose,
                  access   = Access}.

timeout() -> ejabberd_config:get_local_option(tros_auth_validity).
