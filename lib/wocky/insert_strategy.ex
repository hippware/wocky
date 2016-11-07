defmodule Wocky.InsertStrategy do
  @moduledoc false

  use Exref, ignore: [function_name: 0]
  use ExMachina.Strategy, function_name: :insert

  def handle_insert(%{__struct__: module} = record, opts) do
    exports = module.__info__(:functions)
    case Keyword.get(exports, :insert) do
      2 -> module.insert(record, opts)
      1 -> module.insert(record)
      _ -> raise ArgumentError
    end
    record
  end

  def handle_insert(_record, _opts) do
    raise ArgumentError
  end
end
