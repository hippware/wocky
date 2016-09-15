defmodule Wocky.Ejabberd do
  @moduledoc false

  require Record
  import Record, only: [defrecord: 2, extract: 2]

  defrecord :xmlel, extract(:xmlel, from_lib: "exml/include/exml.hrl")
  defrecord :xmlcdata, extract(:xmlcdata, from_lib: "exml/include/exml.hrl")
  defrecord :iq, extract(:iq, from_lib: "ejabberd/include/jlib.hrl")
  defrecord :jid, extract(:jid, from_lib: "ejabberd/include/jlib.hrl")
end
