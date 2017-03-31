defmodule Wocky.JID do
  @moduledoc "TODO"

  import Record, only: [defrecord: 2, extract: 2]

  require Record

  defrecord :jid, extract(:jid, from_lib: "xmpp/include/jid.hrl")

  @type jid :: record(:jid, user: binary,
                            server: binary,
                            resource: binary,
                            luser: binary,
                            lserver: binary,
                            lresource: binary)

  defmacro __using__(_) do
    quote do
      import Wocky.JID
      alias :jid, as: JID
    end
  end
end
