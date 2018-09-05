defmodule Wocky.MessageTest do
  use Wocky.DataCase

  alias Wocky.Message
  alias Wocky.Repo.Factory

  setup do
    [u1, u2, u3] = Factory.insert_list(3, :user)

    messages2 =
      1..5
      |> Enum.map(fn _ -> Factory.insert_message(u1, u2) end)
      |> strip_meta()

    messages3 =
      1..3
      |> Enum.map(fn _ -> Factory.insert_message(u1, u3) end)
      |> strip_meta()

    {:ok,
     user1: u1, user2: u2, user3: u3, messages2: messages2, messages3: messages3}
  end

  test "get_messages should work", %{
    user1: u1,
    messages2: messages2,
    messages3: messages3
  } do
    messages =
      u1
      |> Message.get_query()
      |> Repo.all()
      |> Message.fix()
      |> strip_meta()

    assert messages == messages2 ++ messages3
  end

  test "get_messages should work for a single user", %{
    user1: u1,
    user2: u2,
    messages2: messages2
  } do
    messages =
      u1
      |> Message.get_query(u2)
      |> Repo.all()
      |> Message.fix()
      |> strip_meta()

    assert messages == messages2
  end

  defp strip_meta(objects), do: Enum.map(objects, &Map.drop(&1, [:__meta__]))
end
