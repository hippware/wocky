defmodule Wocky.Watcher do
  @moduledoc """
  Helper module for DB callback modules
  """

  defmacro __using__(opts) do
    quote do
      alias Wocky.Watcher.Client
      alias WockyDBWatcher.Event

      def register do
        Enum.each(unquote(opts[:events]), fn e ->
          fun =
            case e do
              :insert -> &handle_insert/1
              :update -> &handle_update/1
              :delete -> &handle_delete/1
            end

          Client.subscribe(unquote(opts[:type]), e, fun)
        end)
      end

      def handle_insert(event) do
        raise UndefinedFunctionError,
              "#{inspect(__MODULE__)} handle_insert/1 not defined"
      end

      def handle_update(event) do
        raise UndefinedFunctionError,
              "#{inspect(__MODULE__)} handle_update/1 not defined"
      end

      def handle_delete(event) do
        raise UndefinedFunctionError,
              "#{inspect(__MODULE__)} handle_delete/1 not defined"
      end

      defoverridable handle_insert: 1, handle_update: 1, handle_delete: 1
    end
  end
end
