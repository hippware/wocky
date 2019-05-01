defmodule WockyAPI.Resolvers.Testing do
  @moduledoc "Resolver for testing-only operations"

  alias Wocky.Repo.Factory

  def factory_insert(args, _context) do
    true = Confex.get_env(:wocky_api, :allow_factory_insert, false)
    {:ok, Enum.map(args[:input], &do_factory_insert/1)}
  rescue
    e -> {:error, inspect(e) <> " " <> inspect(__STACKTRACE__)}
  end

  defp do_factory_insert(args) do
    # The atom for the factory type may not exist at runtime, so we can't use
    # `to_existing_atom` directly. We can, however, check safety by checking
    # that the factory atom itself exists:
    _type_check = String.to_existing_atom(args[:type] <> "_factory")

    # ...and if it does we can safely create the type atom:
    # credo:disable-for-next-line Credo.Check.Warning.UnsafeToAtom
    type = String.to_atom(args[:type])

    count = args[:count] || 1
    params = normalise_params(args)

    count
    |> Factory.insert_list(type, params)
    |> Enum.map(&to_string(&1.id))
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

  # If a caller supplies, for example, `user_id`, the default factory
  # implementation will still build a value for `user` which will clash. This
  # function adds a `nil` entry for the full object if the id is specified which
  # overrides the default creation.
  defp add_nils({k, v}) do
    k_str = to_string(k)

    if String.ends_with?(k_str, "_id") do
      [{k, v}, {k_str |> String.slice(0..-4) |> String.to_existing_atom(), nil}]
    else
      [{k, v}]
    end
  end
end
