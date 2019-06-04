defmodule Wocky.User.CurrentLocationTest do
  use Wocky.DataCase

  alias Wocky.Repo.{Factory, Timestamp}
  alias Wocky.Roster
  alias Wocky.User
  alias Wocky.User.CurrentLocation

  setup do
    {:ok, users: Factory.insert_list(4, :user)}
  end

  describe "delete_when_not_shared/1" do
    setup %{users: [u1, u2 | _] = users} do
      Roster.befriend(u1, u2)
      {:ok, _} = User.start_sharing_location(u1, u2, Timestamp.shift(days: 1))
      {:ok, _} = User.start_sharing_location(u2, u1, Timestamp.shift(days: 1))
      loc = Factory.build(:location)
      Enum.each(users, &CurrentLocation.set(&1, loc))
      :ok
    end

    test "delete without shares", %{users: [u1, u2, u3, u4]} do
      assert CurrentLocation.delete_when_not_shared([u1.id, u2.id, u3.id]) == 1
      refute is_nil(CurrentLocation.get(u1))
      refute is_nil(CurrentLocation.get(u2))
      assert is_nil(CurrentLocation.get(u3))
      refute is_nil(CurrentLocation.get(u4))
    end
  end
end
