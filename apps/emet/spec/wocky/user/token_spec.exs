defmodule Wocky.User.TokenSpec do
  use ESpec, async: true

  alias Wocky.ID
  alias Wocky.Repo
  alias Wocky.User.Token

  before do
    id = ID.new
    resource = Faker.Code.issn
    result = Token.assign(id, shared.server, resource)
    {:ok, result: result, id: id, resource: resource}
  end

  finally do
    Repo.delete("tokens", shared.server, shared.id)
  end

  describe "new/0" do
    it "should create a token that starts with '$T$'" do
      Token.new |> should(start_with "$T$")
    end
  end

  describe "assign/3" do
    it "should return a success tuple" do
      shared.result |> should(be_ok_result())
    end

    it "should return the generated token" do
      {:ok, {token, _}} = shared.result
      token |> should(start_with "$T$")
    end

    it "should return an expiry value" do
      {:ok, {_, expiry}} = shared.result
      expiry |> should(be_integer())
    end

    it "should store the token for the user" do
      {:ok, {token, expiry}} = shared.result
      map =
        "tokens"
        |> Repo.find(shared.server, shared.id)
        |> Map.get(String.to_atom(shared.resource))

      map[:user_id]    |> should(eq shared.id)
      map[:server]     |> should(eq shared.server)
      map[:resource]   |> should(eq shared.resource)
      map[:token]      |> should(eq token)
      map[:expires_at] |> should(eq to_string(expiry))
    end

    it "should return a different token every time" do
      {:ok, {token1, _}} = shared.result
      {:ok, {token2, _}} =
        Token.assign(shared.id, shared.server, shared.resource)

      token1 |> should_not(eq token2)
    end

    it "should overwrite tokens when there are multiple requests" do
      {:ok, _} = Token.assign(shared.id, shared.server, shared.resource)

      keys =
        "tokens"
        |> Repo.find(shared.server, shared.id)
        |> Map.keys

      keys |> should(eq [String.to_atom(shared.resource)])
    end
  end

  describe "get_token/3" do
    it "should return the token assigned to a resource" do
      {:ok, {token, _}} = shared.result

      shared.id
      |> Token.get_token(shared.server, shared.resource)
      |> should(eq token)
    end

    it "should return nil if the resource is not assigned a token" do
      ID.new
      |> Token.get_token(shared.server, shared.resource)
      |> should(be_nil())

      shared.id
      |> Token.get_token("nosuchserver", shared.resource)
      |> should(be_nil())

      shared.id
      |> Token.get_token(shared.server, "nosuchresource")
      |> should(be_nil())
    end
  end

  describe "get_tokens/2" do
    it "should return a list of tokens for the specified user" do
      {:ok, {token, _}} = shared.result

      shared.id
      |> Token.get_tokens(shared.server)
      |> should(eq [token])
    end

    it "should return an empty list if the user has no assigned tokens" do
      ID.new
      |> Token.get_tokens(shared.server)
      |> should(be_empty())

      shared.id
      |> Token.get_tokens("nosuchserver")
      |> should(be_empty())
    end
  end

  describe "valid?/3" do
    before do
      {:ok, {token, _}} = shared.result
      {:ok, token: token}
    end

    it "should return true for a valid, unexpired token" do
      shared.id
      |> Token.valid?(shared.server, shared.token)
      |> should(be_true())
    end

    it "should return false for an invalid token" do
      shared.id
      |> Token.valid?(shared.server, "nosuchtoken")
      |> should(be_false())
    end

    it "should return false for an expired token" do
      :ok =
        %{}
        |> Map.put(shared.resource, %{expires_at: "100"})
        |> Repo.update("tokens", shared.server, shared.id)

      shared.id
      |> Token.valid?(shared.server, shared.token)
      |> should(be_false())
    end

    it "should return false for a nonexistent user" do
      ID.new
      |> Token.valid?(shared.server, shared.token)
      |> should(be_false())
    end
  end

  describe "release/3" do
    before do
      result = Token.release(shared.id, shared.server, shared.resource)
      {:ok, result: result}
    end

    it "should return :ok" do
      shared.result |> should(eq :ok)
    end

    it "should remove the token from the database" do
      "tokens"
      |> Repo.find(shared.server, shared.id)
      |> Map.get(String.to_atom(shared.resource))
      |> should(be_nil())
    end

    it "should return :ok if the token has already been removed" do
      shared.id
      |> Token.release(shared.server, shared.resource)
      |> should(eq :ok)
    end

    it "should return :ok if there never was a token" do
      shared.id
      |> Token.release(shared.server, "nosuchresource")
      |> should(eq :ok)
    end
  end

  describe "release_all/2" do
    before do
      result = Token.release_all(shared.id, shared.server)
      {:ok, result: result}
    end

    it "should return :ok" do
      shared.result |> should(eq :ok)
    end

    it "should remove all tokens from the database" do
      "tokens"
      |> Repo.find(shared.server, shared.id)
      |> should(be_nil())
    end

    it "should return :ok if the user doesn't have any tokens" do
      ID.new
      |> Token.release_all(shared.server)
      |> should(eq :ok)
    end
  end
end
