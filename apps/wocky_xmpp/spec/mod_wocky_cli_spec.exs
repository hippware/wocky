defmodule :mod_wocky_cli_spec do
  use ESpec, async: true, sandbox: true
  use SandboxHelper

  import :mod_wocky_cli, only: [befriend: 2]

  alias Wocky.Repo.Factory
  alias Wocky.Roster

  before do
    alice = Factory.insert(:user)
    bob = Factory.insert(:user)
    {:ok, alice: alice, bob: bob}
  end

  describe "befriend/2" do
    context "with two valid users" do
      before do
        result = befriend(shared.alice.handle, shared.bob.handle)
        {:ok, result: result}
      end

      it "returns a success result" do
        shared.result |> should(be_ok_result())
      end

      it "should make user A friends with user B" do
        assert Roster.is_friend(shared.alice.id, shared.bob.id)
      end

      it "should make user B friends with user A" do
        assert Roster.is_friend(shared.bob.id, shared.alice.id)
      end
    end

    context "with invalid users" do
      it "returns an error" do
        befriend(shared.alice.handle, "bobbb") |> should(be_error_result())
        befriend("alisher", shared.bob.handle) |> should(be_error_result())
        befriend("xxx", "ffff") |> should(be_error_result())
      end
    end
  end
end
