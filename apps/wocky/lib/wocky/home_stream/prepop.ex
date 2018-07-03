defmodule Wocky.HomeStream.Prepop do
  @moduledoc """
  Module for dealing with the homestream prepopulation archive user.
  """

  import Ecto.Query

  alias Timex.Duration
  alias Wocky.HomeStream
  alias Wocky.HomeStream.Item
  alias Wocky.Repo
  alias Wocky.Roster
  alias Wocky.User

  @spec handle :: User.handle() | nil
  def handle, do: Confex.get_env(:wocky, :hs_prepopulation_user)

  @spec user :: User.t() | nil
  def user, do: Repo.get_by(User, handle: handle() || "")

  @spec add_source(User.handle()) :: :ok
  def add_source(handle) do
    user = Repo.get_by(User, handle: handle)
    Roster.follow(user().id, user.id)
    Roster.add_initial_contact(user, :followee)
  end

  @spec prepopulate(User.id(), Keyword.t()) :: :ok
  def prepopulate(user_id, opts \\ []) do
    opts = Keyword.merge(prepop_defaults(), opts)
    source = Repo.get_by(User, handle: handle())
    prepopulate_from(user_id, source, opts[:period], opts[:min])
  end

  defp prepop_defaults do
    days = Confex.get_env(:wocky, :hs_prepopulation_days)

    [
      period: Duration.from_days(days),
      min: Confex.get_env(:wocky, :hs_prepopulation_min_items)
    ]
  end

  defp prepopulate_from(_user_id, nil, _period, _min), do: :ok

  defp prepopulate_from(user_id, from, period, min) do
    from.id
    |> prepop_items(period, min)
    |> Enum.each(fn i ->
      params =
        i
        |> Map.take(Item.writable_fields())
        |> Map.put(:user_id, user_id)

      %Item{}
      |> Item.changeset(params)
      |> Repo.insert(
        on_conflict: :replace_all,
        conflict_target: [:user_id, :key]
      )
    end)
  end

  defp prepop_items(from_id, period, min) do
    from_time = Timex.subtract(DateTime.utc_now(), period)

    time_items = HomeStream.get_after_time(from_id, from_time, :none, true)

    if length(time_items) < min do
      get_by_count(from_id, min)
    else
      time_items
    end
  end

  defp get_by_count(user_id, count) do
    user_id
    |> HomeStream.get_query(include_deleted: true)
    |> order_by(desc: :updated_at)
    |> limit(^count)
    |> Repo.all()
  end
end
