defmodule :ejabberd_auth_wocky_spec do
  use ESpec, async: true, sandbox: true
  use SandboxHelper

  alias Wocky.Account
  alias Wocky.Repo.ID

  before do
    user = ID.new()
    resource = ID.new()

    {:ok, _} =
      :ejabberd_auth_wocky.try_register(user, shared.server, "password")

    {:ok, {token, _}} = Account.assign_token(user, resource)
    {:ok, user: user, resource: resource, token: token}
  end

  describe "check_password/3 with token" do
    it "should return true when the user exists and token matches" do
      shared.user
      |> :ejabberd_auth_wocky.check_password(shared.server, shared.token)
      |> should(be_true())
    end

    it "should return false when the user exists but the token doesn't match" do
      token = Account.generate_token()
      shared.user
      |> :ejabberd_auth_wocky.check_password(shared.server, token)
      |> should(be_false())
    end

    it "should return false when the user does not exist" do
      ID.new()
      |> :ejabberd_auth_wocky.check_password(shared.server, shared.token)
      |> should(be_false())
    end
  end
end
