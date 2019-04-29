defmodule WockyAPI.Resolvers.Testing do
  @moduledoc "Resolver for testing-only operations"

  alias Wocky.Repo.Factory

  def factory_insert(args, _context) do
    true = Confex.get_env(:wocky_api, :allow_factory_insert, false)
    type = String.to_existing_atom(args[:input][:type])
    count = args[:input][:count] || 1
    params = normalise_params(args[:input])

    ids =
      count
      |> Factory.insert_list(type, params)
      |> Enum.map(&to_string(&1.id))

    {:ok, ids}
  rescue
    e -> {:error, inspect(e) <> " " <> inspect(__STACKTRACE__)}
  end

  defp normalise_params(input) do
    atoms =
      Enum.map(input[:atom_params] || [], fn %{value: v} = p ->
        %{p | value: String.to_existing_atom(v)}
      end)

    [:bool_params, :string_params, :int_params, :float_params]
    |> Enum.map(&(input[&1] || []))
    |> Enum.concat()
    |> Enum.concat(atoms)
    |> Enum.map(fn %{key: k, value: v} -> {String.to_existing_atom(k), v} end)
    |> Enum.flat_map(&add_nils/1)
  end

  defp add_nils({k, v}) do
    k_str = to_string(k)

    if String.ends_with?(k_str, "_id") do
      [{k, v}, {k_str |> String.slice(0..-4) |> String.to_existing_atom(), nil}]
    else
      [{k, v}]
    end
  end
end
