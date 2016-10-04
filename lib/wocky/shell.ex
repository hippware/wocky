defmodule Wocky.Shell do
  @moduledoc false

  use Exref, ignore: [q: 0]

  @spec q :: no_return
  def q do
    _ = :wocky_app.stop
    System.halt(0)
  end
end
