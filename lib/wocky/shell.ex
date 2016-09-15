defmodule Wocky.Shell do
  @moduledoc false

  def q do
    :wocky_app.stop
    System.halt(0)
  end
end
