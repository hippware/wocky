defmodule Wocky.Tasks.Weekly do
  @moduledoc "The tasks to run on a weekly basis"

  alias Wocky.Bot.Report
  alias Wocky.Repo.Cleaner

  def run do
    :ok = Application.ensure_all_started(:wocky)

    _ = Cleaner.clean_all
    _ = Report.run

    :init.stop
  end
end
