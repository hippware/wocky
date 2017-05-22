defmodule Wocky.Shell do
  def q do
    _ = :wocky_xmpp_app.stop
    System.halt(0)
  end
end

import Wocky.Shell
