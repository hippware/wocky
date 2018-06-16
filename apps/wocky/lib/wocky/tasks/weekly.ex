defmodule Wocky.Tasks.Weekly do
  @moduledoc "The tasks to run on a weekly basis"

  alias Wocky.Repo.Cleaner

  def run do
    {:ok, _} = Application.ensure_all_started(:wocky)

    _ = Cleaner.clean_all()

    :init.stop()
  end
end
