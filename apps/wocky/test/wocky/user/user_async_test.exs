defmodule Wocky.User.UserAsyncTest do
  use Wocky.DataCase, async: false

  import Mock

  alias Wocky.{Block, User}
  alias Wocky.Repo.Factory

  setup do
    user = Factory.insert(:user, device: "testing", first_name: "first", last_name: "last", handle: "handle")

    {:ok,
     user: user,
     id: user.id,
     external_id: user.external_id,
     phone_number: user.phone_number}
  end

  describe "user deletion" do
    setup_with_mocks([
      {FirebaseAdminEx.Auth, [], [delete_user: fn _ -> :ok end]}
    ]) do
      :ok
    end

    test "should delete user's firebase account if they have one" do
      user = Factory.insert(:user, provider: "firebase")
      assert User.delete(user.id) == :ok

      assert_called FirebaseAdminEx.Auth.delete_user(user.external_id)
    end

    test "should not delete user's firebase account if they don't have one" do
      user = Factory.insert(:user)
      assert User.delete(user.id) == :ok

      refute called FirebaseAdminEx.Auth.delete_user(user.external_id)
    end
  end

  describe "search_by_name/3" do
    setup do
      users =
        [
          {"Alice", "Sanders", "Xena"},
          {"Alison", "Smith", "Yaniv"},
          {"Bob", "Jones", "Zena"},
          {"acéñtîâ", "CAPITAL", "1345"}
        ]
        |> Enum.map(fn {f, l, h} ->
          Factory.insert(:user, first_name: f, last_name: l, handle: h)
        end)

      {:ok, users: users}
    end

    test "should return all users with the search prefix in either name", ctx do
      assert User.search_by_name("a", ctx.id, 50) |> length() == 3
      assert User.search_by_name("b", ctx.id, 50) |> length() == 1
      assert User.search_by_name("s", ctx.id, 50) |> length() == 2
      assert User.search_by_name("smi", ctx.id, 50) |> length() == 1
      assert User.search_by_name("q", ctx.id, 50) |> length() == 0
      assert User.search_by_name("z", ctx.id, 50) |> length() == 1
      assert User.search_by_name("13", ctx.id, 50) |> length() == 1
    end

    test "should ignore accents in both search and data", ctx do
      assert User.search_by_name("acent", ctx.id, 50) |> length() == 1
      assert User.search_by_name("â", ctx.id, 50) |> length() == 3
    end

    test "should ignore capitalisation in both search and data", ctx do
      assert User.search_by_name("A", ctx.id, 50) |> length() == 3
      assert User.search_by_name("c", ctx.id, 50) |> length() == 1
    end

    test "should respect the limit parameter", ctx do
      assert User.search_by_name("a", ctx.id, 2) |> length() == 2
    end

    test "should ignore empty search terms and return an empty list", ctx do
      assert User.search_by_name("", ctx.id, 50) |> length() == 0
    end

    test "should work on multiple partial terms", ctx do
      assert User.search_by_name("ali s", ctx.id, 50) |> length() == 2
      assert User.search_by_name("ali sm", ctx.id, 50) |> length() == 1
    end

    test "should not choke on punctuation or other unicode weirdness", ctx do
      assert User.search_by_name("''ali", ctx.id, 50) |> length() == 2
      assert User.search_by_name("al-s", ctx.id, 50) |> length() == 0
      assert User.search_by_name("al''i", ctx.id, 50) |> length() == 2
      assert User.search_by_name("al''i", ctx.id, 50) |> length() == 2
      assert User.search_by_name("''-al''i", ctx.id, 50) |> length() == 2
    end

    test "should not return a blocking user", ctx do
      # Alice Sanders
      blocking_user = hd(ctx.users)
      Block.block(blocking_user, ctx.user)

      result = User.search_by_name("a", ctx.id, 50)

      assert length(result) == 2
      refute Enum.any?(result, fn %{id: id} -> id == blocking_user.id end)
    end
  end
end

