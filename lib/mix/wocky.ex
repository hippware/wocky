defmodule Mix.Wocky do
  @moduledoc false

  def start_app do
    # Don't start ejabberd
    System.put_env("WOCKY_MINIMAL", "1")

    # Start the application
    Mix.shell.print_app
    Mix.Task.run "app.start"

    # Force the lager console to info
    :lager.set_loglevel(:lager_console_backend, :info)
  end

  def set_error_exit(set?) do
    System.at_exit(fn _ -> if set?, do: exit({:shutdown, 1}) end)
  end
end
