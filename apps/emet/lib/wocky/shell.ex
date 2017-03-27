defmodule Wocky.Shell do
  @moduledoc false

  @spec q :: no_return
  def q do
    _ = :wocky_app.stop
    System.halt(0)
  end
end
