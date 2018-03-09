defmodule Wocky.Account.TokenTest do
  use Wocky.DataCase

  alias Comeonin.Bcrypt
  alias Wocky.Account.Token
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID

  setup do
    user = Factory.insert(:user)
    resource = Faker.Code.issn()
    {:ok, id: user.id, resource: resource}
  end

  defp token_from_context(ctx) do
    Repo.one(
      from t in Token,
      where: t.user_id == ^ctx.id,
      where: t.resource == ^ctx.resource
    )
  end

  describe "generate/0" do
    test "should create a token that starts with '$T$'" do
      assert Token.generate() |> String.starts_with?("$T$")
    end
  end

  describe "assign/2" do
    test "should return the token and expiry", ctx do
      {:ok, {token, expiry}} = Token.assign(ctx.id, ctx.resource)

      assert String.starts_with?(token, "$T$")
      assert %DateTime{} = expiry
    end

    test "should store the token for the user", ctx do
      {:ok, {token, expiry}} = Token.assign(ctx.id, ctx.resource)

      data = token_from_context(ctx)

      assert data.user_id == ctx.id
      assert data.resource == ctx.resource
      assert data.expires_at == expiry
      assert Bcrypt.checkpw(token, data.token_hash)
    end

    test "should return a different token every time", ctx do
      {:ok, {token1, _}} = Token.assign(ctx.id, ctx.resource)
      {:ok, {token2, _}} = Token.assign(ctx.id, ctx.resource)

      refute token1 == token2
    end

    test "should overwrite tokens when there are multiple requests", ctx do
      {:ok, {token, expiry}} = Token.assign(ctx.id, ctx.resource)
      {:ok, _} = Token.assign(ctx.id, ctx.resource)

      data = token_from_context(ctx)

      assert data.user_id == ctx.id
      assert data.resource == ctx.resource
      refute data.expires_at == to_string(expiry)
      refute Bcrypt.checkpw(token, data.token_hash)
    end
  end

  describe "valid?/2" do
    setup ctx do
      {:ok, {token, _}} = Token.assign(ctx.id, ctx.resource)
      {:ok, token: token}
    end

    test "should return true for a valid, unexpired token", ctx do
      Token.valid?(ctx.id, ctx.token)
    end

    test "should return false for an invalid token", ctx do
      refute Token.valid?(ctx.id, "nosuchtoken")
    end

    test "should return false for an expired token", ctx do
      ctx
      |> token_from_context()
      |> Token.changeset(%{expires_at: DateTime.utc_now()})
      |> Repo.update()

      refute Token.valid?(ctx.id, ctx.token)
    end

    test "should return false for a nonexistent user", ctx do
      refute Token.valid?(ID.new(), ctx.token)
    end
  end

  describe "release/2" do
    setup ctx do
      {:ok, {_, _}} = Token.assign(ctx.id, ctx.resource)
      :ok
    end

    test "should remove the token from the database", ctx do
      assert :ok == Token.release(ctx.id, ctx.resource)

      refute token_from_context(ctx)
    end

    test "should return :ok if the token has already been removed", ctx do
      assert :ok == Token.release(ctx.id, ctx.resource)
      assert :ok == Token.release(ctx.id, ctx.resource)
    end

    test "should return :ok if there never was a token", ctx do
      assert :ok == Token.release(ctx.id, "nosuchresource")
    end
  end

  describe "release_all/1" do
    test "should remove all tokens from the database", ctx do
      assert :ok == Token.release_all(ctx.id)

      query = from t in Token, where: t.user_id == ^ctx.id

      assert Repo.all(query) == []
    end

    test "should return :ok if the user doesn't have any tokens" do
      assert :ok == Token.release_all(ID.new())
    end
  end
end
