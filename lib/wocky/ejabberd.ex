defmodule Wocky.Ejabberd do
  @moduledoc false

  use Exref, ignore: [
    'MACRO-iq': 1, 'MACRO-iq': 2, 'MACRO-iq': 3,
    'MACRO-xmlel': 1, 'MACRO-xmlel': 2, 'MACRO-xmlel': 3,
    'MACRO-xmlcdata': 1, 'MACRO-xmlcdata': 2, 'MACRO-xmlcdata': 3
  ]

  require Record
  import Record, only: [defrecord: 2, extract: 2]

  defrecord :xmlel, extract(:xmlel, from_lib: "exml/include/exml.hrl")
  defrecord :xmlcdata, extract(:xmlcdata, from_lib: "exml/include/exml.hrl")
  defrecord :iq, extract(:iq, from_lib: "ejabberd/include/jlib.hrl")

  defmacro __using__(_) do
    quote do
      import Wocky.Ejabberd
      alias Wocky.Ejabberd
    end
  end
end
