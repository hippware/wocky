defmodule Wocky.Ejabberd do
  @moduledoc false

  import Record, only: [defrecord: 2, extract: 2]

  @type jid :: record(:jid, user: :ejabberd.user,
                            server: :ejabberd.server,
                            resource: :ejabberd.resource,
                            luser: :ejabberd.luser,
                            lserver: :ejabberd.lserver,
                            lresource: :ejabberd.lresource)

  defrecord :xmlel, extract(:xmlel, from_lib: "exml/include/exml.hrl")
  defrecord :xmlcdata, extract(:xmlcdata, from_lib: "exml/include/exml.hrl")
  defrecord :iq, extract(:iq, from_lib: "ejabberd/include/jlib.hrl")
  defrecord :jid, extract(:jid, from_lib: "ejabberd/include/jlib.hrl")

  defmacro __using__(_) do
    quote do
      import Wocky.Ejabberd
      alias Wocky.Ejabberd
      alias :jid, as: Jid
    end
  end

  @spec make_jid!(:ejabberd.luser, :ejabberd.lserver, :ejabberd.lresource) ::
    jid
  def make_jid!(user, server, resource \\ "") do
    case :jid.make(user, server, resource) do
      :error -> raise ArgumentError
      jid -> jid
    end
  end

  @spec make_jid!(binary) :: jid
  def make_jid!(jidstring) do
    case :jid.from_binary(jidstring) do
      :error -> raise ArgumentError
      jid -> jid
    end
  end

end
