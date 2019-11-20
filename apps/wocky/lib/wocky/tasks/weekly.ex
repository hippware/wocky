defmodule Wocky.Tasks.Weekly do
  @moduledoc "The tasks to run on a weekly basis"

  alias Wocky.Repo.Cleaner

  @spec run :: :ok
  def run do
    {:ok, _} = Wocky.start_db_only()

    _ = Cleaner.clean_all()

    :init.stop()
  end
end
