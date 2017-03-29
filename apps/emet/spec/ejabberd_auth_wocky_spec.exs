defmodule EjabberdAuthWockySpec do
  use ESpec, async: true

  alias Golem.ID
  alias Wocky.User.Token
  alias :ejabberd_auth_wocky, as: Auth

  describe "check_password/3 with token" do
    before do
      user = ID.new
      resource = ID.new
      :ok = Auth.try_register(user, shared.server, "password")
      {:ok, {token, _}} = Token.assign(user, shared.server, resource)
      {:ok, user: user, resource: resource, token: token}
    end

    finally do
      Token.release(shared.user, shared.server, shared.resource)
      Auth.remove_user(shared.user, shared.server)
    end

    it "should return true when the user exists and token matches" do
      shared.user
      |> Auth.check_password(shared.server, shared.token)
      |> should(be_true())
    end

    it "should return false when the user exists but the token doesn't match" do
      shared.user
      |> Auth.check_password(shared.server, Token.new)
      |> should(be_false())
    end

    it "should return false when the user does not exist" do
      ID.new
      |> Auth.check_password(shared.server, shared.token)
      |> should(be_false())
    end
  end
end
