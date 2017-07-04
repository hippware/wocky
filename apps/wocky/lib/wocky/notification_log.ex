defmodule Wocky.NotificationLog do
  @moduledoc """
  DB interface module for push notification logging
  """

  use Wocky.Repo.Model

  alias Wocky.Repo
  alias Wocky.User

  alias __MODULE__, as: NotificationLog

  @foreign_key_type :binary_id
  schema "notification_logs" do
    field :resource,  :string
    field :message,   :string
    field :reference, :binary
    field :result,    :boolean
    field :details,   :string

    belongs_to :user, User

    timestamps()
  end

  @type t :: %NotificationLog{
    user_id:    User.id,
    resource:   User.resource,
    message:    binary,
    reference:  binary,
    result:     boolean,
    details:    binary
  }

  @spec send(reference, User.user, User.resource, binary) :: :ok
  def send(reference, user, resource, body) do
    params = %{user_id: user,
               resource: resource,
               message: body,
               reference: :erlang.term_to_binary(reference)}

    %NotificationLog{}
    |> cast(params, [:user_id, :resource, :message, :reference])
    |> Repo.insert!
    :ok
  end

  @spec result(reference, boolean, binary) :: :ok
  def result(reference, result, details \\ "") do
    reference_bin = :erlang.term_to_binary(reference)
    subquery = latest_entry_subquery(reference_bin)

    NotificationLog
    |> join(:inner, [n], r in subquery(subquery))
    |> where([n, r], n.user_id == r.user_id)
    |> where([n, r], n.reference == r.reference)
    |> where([n, r], n.created_at == r.created_at)
    |> Repo.update_all(set: [result: result, details: details])
    :ok
  end

  # References are only uniuqe withing a given VM run, so to be certain
  # we update the right one, we need to select only the most recent
  # occurence
  defp latest_entry_subquery(reference_bin) do
    NotificationLog
    |> select([:user_id, :reference, :created_at])
    |> where(reference: ^reference_bin)
    |> order_by([desc: :created_at])
    |> limit(1)
  end
end
