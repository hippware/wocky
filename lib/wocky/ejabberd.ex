defmodule Wocky.Ejabberd do
  @moduledoc false

  use Exref, ignore: [
    'MACRO-iq': 1, 'MACRO-iq': 2, 'MACRO-iq': 3,
    'MACRO-jid': 1, 'MACRO-jid': 2, 'MACRO-jid': 3,
    'MACRO-xmlel': 1, 'MACRO-xmlel': 2, 'MACRO-xmlel': 3,
    'MACRO-xmlcdata': 1, 'MACRO-xmlcdata': 2, 'MACRO-xmlcdata': 3
  ]

  require Record
  import Record, only: [defrecord: 2, extract: 2]

  defrecord :xmlel, extract(:xmlel, from_lib: "exml/include/exml.hrl")
  defrecord :xmlcdata, extract(:xmlcdata, from_lib: "exml/include/exml.hrl")
  defrecord :iq, extract(:iq, from_lib: "ejabberd/include/jlib.hrl")
  defrecord :jid, extract(:jid, from_lib: "ejabberd/include/jlib.hrl")

  @type jid :: record(:jid, user: :ejabberd.user,
                            server: :ejabberd.server,
                            resource: :ejabberd.resource,
                            luser: :ejabberd.luser,
                            lserver: :ejabberd.lserver,
                            lresource: :ejabberd.lresource)

  defmacro __using__(_) do
    quote do
      import Wocky.Ejabberd
      alias Wocky.Ejabberd
      alias :jid, as: Jid
    end
  end

  def make_jid!(user, server, resource \\ "") do
    case :jid.make(user, server, resource) do
      :error -> raise ArgumentError
      jid -> jid
    end
  end

  def make_jid!(jidstring) do
    case :jid.from_binary(jidstring) do
      :error -> raise ArgumentError
      jid -> jid
    end
  end

end
