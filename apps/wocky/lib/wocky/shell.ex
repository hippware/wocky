defmodule Wocky.Shell do
  @moduledoc false

  @spec q :: no_return
  def q do
    _ = Application.stop(:wocky_xmpp)
    _ = Application.stop(:wocky)
    System.halt(0)
  end
end
