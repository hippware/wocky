defmodule Wocky.Honeybadger do
  @moduledoc """
    This module provides trivial wrappings to the Honeybadger client module.
    This is required to allow us to call 'notify' from Erlang, since
    Honeybadger implements it as a macro.
  """

  require Honeybadger

  use Exref, ignore: [notify: 1, notify: 2, notify: 3, context: 1]

  @spec notify(binary) :: :ok
  def notify(exception) do
    {:ok, _} = Honeybadger.notify(exception)
    :ok
  end

  @spec notify(binary, map) :: :ok
  def notify(exception, metadata) do
    {:ok, _} = Honeybadger.notify(exception, metadata)
    :ok
  end

  @spec notify(binary, map, list) :: :ok
  def notify(exception, metadata, stacktrace) do
    {:ok, _} = Honeybadger.notify(exception, metadata, stacktrace)
    :ok
  end

  @spec context(map) :: :ok
  def context(dict) do
    _ = Honeybadger.context(dict)
    :ok
  end
end
