defmodule Wocky.Honeybadger do
  @moduledoc """
    This module provides trivial wrappings to the Honeybadger client module.
    This is required to allow us to call 'notify' from Erlang, since
    Honeybadger implements it as a macro.
  """

  require Honeybadger

  use Exref, ignore: [notify: 1, notify: 2, notify: 3, context: 1]

  def notify(exception) do
    Honeybadger.notify(exception)
  end

  def notify(exception, metadata) do
    Honeybadger.notify(exception, metadata)
  end

  def notify(exception, metadata, stacktrace) do
    Honeybadger.notify(exception, metadata, stacktrace)
  end

  def context(dict) do
    Honeybadger.context(dict)
  end
end
