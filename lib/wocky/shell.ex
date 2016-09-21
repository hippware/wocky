defmodule Wocky.Shell do
  @moduledoc false

  use Exref, ignore: [q: 0]

  def q do
    :wocky_app.stop
    System.halt(0)
  end
end
