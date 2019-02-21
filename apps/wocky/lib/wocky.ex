defmodule Wocky do
  @moduledoc """
  Wocky keeps the contexts that define your domain
  and business logic.

  Contexts are also responsible for managing your data, regardless
  if it comes from the database, an external API or others.
  """

  def start_db_only do
    Application.put_env(:wocky, :db_only_mode, true)
    Application.ensure_all_started(:wocky)
  end

  def host, do: Confex.get_env(:wocky, :wocky_host)
end
