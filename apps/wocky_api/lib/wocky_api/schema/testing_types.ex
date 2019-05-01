defmodule WockyAPI.Schema.TestingTypes do
  @moduledoc """
  Absinthe types for testing operations. These operations should not be enabled
  in a production environment.
  """

  use WockyAPI.Schema.Notation
  import Kronky.Payload

  alias WockyAPI.Resolvers.Testing

  input_object :factory_insert_input do
    field :count, :integer

    field :type, non_null(:string)

    field :string_params, list_of(:string_param)
    field :int_params, list_of(:int_param)
    field :float_params, list_of(:float_param)
    field :bool_params, list_of(:bool_param)
    field :atom_params, list_of(:atom_param)
  end

  input_object :string_param do
    field :key, non_null(:string)
    field :value, non_null(:string)
  end

  input_object :int_param do
    field :key, non_null(:string)
    field :value, non_null(:integer)
  end

  input_object :float_param do
    field :key, non_null(:string)
    field :value, non_null(:float)
  end

  input_object :bool_param do
    field :key, non_null(:string)
    field :value, non_null(:boolean)
  end

  input_object :atom_param do
    field :key, non_null(:string)
    field :value, non_null(:string)
  end

  payload_object(:factory_insert_payload, list_of(list_of(:string)))

  object :testing_mutations do
    field :factory_insert, type: :factory_insert_payload do
      arg :input, list_of(non_null(:factory_insert_input))
      resolve &Testing.factory_insert/2
      middleware &build_payload/2
    end
  end
end
