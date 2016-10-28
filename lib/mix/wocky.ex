defmodule Mix.Wocky do
  @moduledoc false

  def start_app(args) do
    # Don't start ejabberd
    System.put_env("WOCKY_MINIMAL", "1")

    # Start the application
    Mix.shell.print_app
    Mix.Task.run "app.start", args
  end

  def set_error_exit(set? \\ true) do
    System.at_exit(fn _ -> if set?, do: exit({:shutdown, 1}) end)
  end

  def set_loglevel(level) do
    :lager.set_loglevel(:lager_console_backend, level)
    Logger.configure(level: level)
  end
end
