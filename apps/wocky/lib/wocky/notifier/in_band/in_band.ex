defmodule Wocky.Notifier.InBand do
  @moduledoc "Implementation of in-band notifications"

  @behaviour Wocky.Notifier

  use Wocky.Context

  alias Wocky.Account.User
  alias Wocky.Errors
  alias Wocky.Notifier.InBand.Event
  alias Wocky.Notifier.InBand.Notification

  @impl true
  def notify(event) do
    type = Event.event_type(event)
    required = Event.required_fields(event)

    params =
      event
      |> Event.transform()
      |> Map.put(:type, type)

    changeset = Notification.changeset(%Notification{}, params, required)

    Errors.log_on_failure(
      "Inserting event of type #{to_string(type)}",
      fn -> Repo.insert(changeset) end
    )
  end

  @spec user_query(
          User.t(),
          Notification.id() | nil,
          Notification.id() | nil,
          [Notification.NotificationTypeEnum.t()] | nil
        ) :: Queryable.t()
  def user_query(user, before_id, after_id, types \\ nil) do
    user
    |> user_query()
    |> maybe_add_type_filter(types)
    |> maybe_add_before_id(before_id)
    |> maybe_add_after_id(after_id)
  end

  defp user_query(user) do
    from n in assoc(user, :notifications),
      preload: [:other_user]
  end

  defp maybe_add_type_filter(queryable, nil), do: queryable

  defp maybe_add_type_filter(queryable, types) do
    from n in queryable,
      where: n.type in ^types
  end

  defp maybe_add_before_id(queryable, nil), do: queryable

  defp maybe_add_before_id(queryable, id) do
    from n in queryable,
      where: n.id < ^id
  end

  defp maybe_add_after_id(queryable, nil), do: queryable

  defp maybe_add_after_id(queryable, id) do
    from n in queryable,
      where: n.id > ^id
  end

  @spec delete(Notification.id() | User.t(), User.t()) :: :ok
  def delete(id, requestor) when is_integer(id) do
    Repo.delete_all(
      from n in assoc(requestor, :notifications),
        where: n.id == ^id
    )

    :ok
  end

  def delete(%User{} = user, other_user) do
    Repo.delete_all(
      from n in assoc(user, :notifications),
        where: n.other_user_id == ^other_user.id
    )

    :ok
  end
end
