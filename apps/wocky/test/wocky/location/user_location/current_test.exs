defmodule Wocky.Location.UserLocation.CurrentTest do
  use Wocky.DataCase, async: true

  alias Wocky.Contacts
  alias Wocky.Location.UserLocation.Current
  alias Wocky.Repo.Factory

  setup do
    {:ok, users: Factory.insert_list(4, :user)}
  end

  describe "delete_when_not_shared/1" do
    setup %{users: [u1, u2 | _] = users} do
      :ok = Contacts.befriend(u1, u2, :always)

      loc = Factory.build(:location)
      Enum.each(users, &Current.set(&1, loc))
      :ok
    end

    test "delete without shares", %{users: [u1, u2, u3, u4]} do
      assert Current.delete_when_not_shared([u1.id, u2.id, u3.id]) == 1
      refute {:ok, nil} == Current.get(u1)
      refute {:ok, nil} == Current.get(u2)
      assert {:ok, nil} == Current.get(u3)
      refute {:ok, nil} == Current.get(u4)
    end
  end
end
