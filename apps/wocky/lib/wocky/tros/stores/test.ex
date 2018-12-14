defmodule Wocky.TROS.Store.Test do
  @moduledoc "Test backend for the TROS file management system"

  alias Wocky.TROS

  @behaviour TROS

  defmacro record(params) do
    quote do
      record_action(__ENV__.function |> elem(0), unquote(params))
    end
  end

  def delete(file_id) do
    record(file_id)
    :ok
  end

  def make_upload_response(reference_url, file_id, _size, _metadata) do
    record(file_id)
    url = "http://localhost/some/file/location"

    resp_fields = resp_fields(:put, url, reference_url)

    {[], resp_fields}
  end

  def get_download_url(_metadata, file_name) do
    record(file_name)
    "https://" <> Wocky.host() <> "/" <> file_name
  end

  defp resp_fields(method, url, reference_url) do
    [
      {"method", method |> to_string |> String.upcase()},
      {"url", url},
      {"reference_url", reference_url}
    ]
  end

  defp record_action(function, params) do
    if :ets.info(__MODULE__, :id) == :undefined,
      do: :ets.new(__MODULE__, [:public, :named_table, :ordered_set])

    :ets.insert(
      __MODULE__,
      {:erlang.unique_integer([:monotonic]), function, params}
    )
  end

  def get_actions do
    __MODULE__
    |> :ets.tab2list()
    |> Enum.map(fn a ->
      a
      |> Tuple.to_list()
      |> tl()
      |> List.to_tuple()
    end)
  end

  def clear_actions, do: :ets.delete_all_objects(__MODULE__)
end
