defmodule Wocky.TokenSpec do
  use ESpec, async: true
  use ModelHelpers

  alias Comeonin.Bcrypt
  alias Wocky.Repo.ID
  alias Wocky.Token

  before do
    user = Factory.insert(:user, %{server: shared.server})
    resource = Faker.Code.issn()
    result = Token.assign(user.id, resource)
    {:ok, result: result, id: user.id, resource: resource}
  end

  finally do
    Repo.delete_all(
      from t in Token,
        where: t.user_id == ^shared.id,
        where: t.resource == ^shared.resource
    )
  end

  describe "generate/0" do
    it "should create a token that starts with '$T$'" do
      Token.generate() |> should(start_with "$T$")
    end
  end

  describe "assign/2" do
    it "should return a success tuple" do
      shared.result |> should(be_ok_result())
    end

    it "should return the generated token" do
      {:ok, {token, _}} = shared.result
      token |> should(start_with "$T$")
    end

    it "should return an expiry value" do
      {:ok, {_, expiry}} = shared.result
      expiry |> should(be_struct DateTime)
    end

    it "should store the token for the user" do
      {:ok, {token, expiry}} = shared.result

      data =
        Repo.one(
          from t in Token,
            where: t.user_id == ^shared.id,
            where: t.resource == ^shared.resource
        )

      data.user_id |> should(eq shared.id)
      data.resource |> should(eq shared.resource)
      data.expires_at |> should(eq expiry)
      Bcrypt.checkpw(token, data.token_hash) |> assert
    end

    it "should return a different token every time" do
      {:ok, {token1, _}} = shared.result
      {:ok, {token2, _}} = Token.assign(shared.id, shared.resource)

      token1 |> should_not(eq token2)
    end

    it "should overwrite tokens when there are multiple requests" do
      {:ok, {token, expiry}} = shared.result
      {:ok, _} = Token.assign(shared.id, shared.resource)

      data =
        Repo.one(
          from t in Token,
            where: t.user_id == ^shared.id,
            where: t.resource == ^shared.resource
        )

      data.user_id |> should(eq shared.id)
      data.resource |> should(eq shared.resource)
      data.expires_at |> should_not(eq to_string(expiry))
      Bcrypt.checkpw(token, data.token_hash) |> refute
    end
  end

  describe "valid?/2" do
    before do
      {:ok, {token, _}} = shared.result
      {:ok, token: token}
    end

    it "should return true for a valid, unexpired token" do
      shared.id
      |> Token.valid?(shared.token)
      |> should(be_true())
    end

    it "should return false for an invalid token" do
      shared.id
      |> Token.valid?("nosuchtoken")
      |> should(be_false())
    end

    it "should return false for an expired token" do
      query =
        from t in Token,
          where: t.user_id == ^shared.id,
          where: t.resource == ^shared.resource

      query
      |> Repo.one()
      |> Token.changeset(%{expires_at: DateTime.utc_now()})
      |> Repo.update()

      shared.id
      |> Token.valid?(shared.token)
      |> should(be_false())
    end

    it "should return false for a nonexistent user" do
      ID.new()
      |> Token.valid?(shared.token)
      |> should(be_false())
    end
  end

  describe "release/2" do
    before do
      result = Token.release(shared.id, shared.resource)
      {:ok, result: result}
    end

    it "should return :ok" do
      shared.result |> should(eq :ok)
    end

    it "should remove the token from the database" do
      query =
        from t in Token,
          where: t.user_id == ^shared.id,
          where: t.resource == ^shared.resource

      query
      |> Repo.one()
      |> should(be_nil())
    end

    it "should return :ok if the token has already been removed" do
      shared.id
      |> Token.release(shared.resource)
      |> should(eq :ok)
    end

    it "should return :ok if there never was a token" do
      shared.id
      |> Token.release("nosuchresource")
      |> should(eq :ok)
    end
  end

  describe "release_all/1" do
    before do
      result = Token.release_all(shared.id)
      {:ok, result: result}
    end

    it "should return :ok" do
      shared.result |> should(eq :ok)
    end

    it "should remove all tokens from the database" do
      query = from t in Token, where: t.user_id == ^shared.id

      query
      |> Repo.all()
      |> should(be_empty())
    end

    it "should return :ok if the user doesn't have any tokens" do
      ID.new()
      |> Token.release_all()
      |> should(eq :ok)
    end
  end
end
