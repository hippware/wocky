defmodule Wocky.GothConfig do
  @moduledoc """
  Callback config module to handle `goth`'s JSON config
  """

  use Goth.Config

  def init(config) do
    key = Confex.get_env(:wocky, :goth_private_key)

    json =
      :wocky
      |> Confex.get_env(:goth_config)
      |> Map.put(:private_key, key)
      |> Poison.encode!()

    {:ok, Keyword.put(config, :json, json)}
  end
end
