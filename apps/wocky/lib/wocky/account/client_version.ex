defmodule Wocky.Account.ClientVersion do
  @moduledoc "Schema for tracking client versions"

  use Wocky.Repo.Schema

  alias Wocky.Repo
  alias Wocky.User

  @foreign_key_type :binary_id
  @primary_key false
  schema "client_versions" do
    field :user_id, :binary_id, null: false, primary_key: true
    field :device, :string, null: false, primary_key: true
    field :version, :string, null: false
    field :attributes, {:array, :string}, null: false, default: []

    timestamps()

    belongs_to :user, User, define_field: false
  end

  @agent_rx ~r/TinyRobot\/(\d+\.\d+\.\d+)(?: \((.*)\))?/
  @record_fields [:user_id, :device, :version, :attributes]

  def record(user, device, agent_str) do
    with {:ok, version, attrs} <- parse_agent(agent_str) do
      %ClientVersion{}
      |> changeset(%{
        user_id: user.id,
        device: device,
        version: version,
        attributes: attrs
      })
      |> Repo.insert!(
        on_conflict: :replace_all,
        conflict_target: [:user_id, :device]
      )
    end
  end

  def changeset(struct, params) do
    struct
    |> cast(params, @record_fields)
    |> validate_required(@record_fields)
    |> unique_constraint(:device, name: :PRIMARY)
  end

  def supported?(nil), do: false

  def supported?(agent_str) do
    case parse_agent(agent_str) do
      {:ok, version, attrs} -> supported?(version, attrs)
      {:error, _} -> false
    end
  end

  defp parse_agent(agent_str) do
    case Regex.run(@agent_rx, agent_str) do
      nil -> {:error, :unknown_client}
      [_, version] -> {:ok, version, []}
      [_, version, attrs] -> {:ok, version, parse_attrs(attrs)}
    end
  end

  defp parse_attrs(""), do: []

  defp parse_attrs(attrs) do
    attrs
    |> String.split(";", trim: true)
    |> Enum.each(&String.trim/1)
  end

  defp supported?(_version, _attrs) do
    # Always return true for now
    true
  end
end
