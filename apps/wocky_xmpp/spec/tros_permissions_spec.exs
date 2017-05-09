defmodule :tros_permissions_spec do
  use ESpec, async: true, sandbox: true
  use SandboxHelper
  use Wocky.JID

  import :tros_permissions, only: [can_download: 3]

  alias Wocky.Repo.Factory
  alias Wocky.User

  before do
    alice = Factory.insert(:user)
    bob = Factory.insert(:user)
    carol = Factory.insert(:user)
    robert = Factory.insert(:user)
    karen = Factory.insert(:user)
    tim = Factory.insert(:user)

    Factory.insert(:roster_item, user: alice, contact: bob)
    Factory.insert(:roster_item, user: alice, contact: carol)
    Factory.insert(:roster_item, user: alice, contact: robert)
    Factory.insert(:roster_item, user: alice, contact: karen)

    {:ok, [
        alice: alice,
        bob: bob,
        carol: carol,
        robert: robert,
        karen: karen,
        tim: tim
      ]}
  end

  describe "download permissions" do
    let :owner, do: shared.alice.id

    describe "user access" do
      let :access, do: "user:" <> (shared.bob |> User.to_jid |> JID.to_binary)

      it "should allow the owner to download" do
        assert can_download(User.to_jid(shared.alice), owner(), access())
      end

      it "should allow the specified user to download" do
        assert can_download(User.to_jid(shared.bob), owner(), access())
      end

      it "should not allow anyone else to download" do
        can_download(User.to_jid(shared.robert), owner(), access())
        |> should(eq {false, :permission_denied})

        can_download(User.to_jid(shared.carol), owner(), access())
        |> should(eq {false, :permission_denied})
      end
    end

    describe "friends access" do
      let :access,
        do: "friends:" <> (shared.alice |> User.to_jid |> JID.to_binary)

      it "should allow the owner to download" do
        assert can_download(User.to_jid(shared.alice), owner(), access())
      end

      it "should allow friends of the specified user to download" do
        assert can_download(User.to_jid(shared.bob), owner(), access())
        assert can_download(User.to_jid(shared.carol), owner(), access())
        assert can_download(User.to_jid(shared.robert), owner(), access())
        assert can_download(User.to_jid(shared.karen), owner(), access())
      end

      it "should not allow anyone else to download" do
        can_download(User.to_jid(shared.tim), owner(), access())
        |> should(eq {false, :permission_denied})
      end

      describe "all access" do
        let :access, do: "all"

        it "should allow the owner to download" do
          assert can_download(User.to_jid(shared.alice), owner(), access())
        end

        it "should allow everyone else to download" do
          assert can_download(User.to_jid(shared.bob), owner(), access())
          assert can_download(User.to_jid(shared.carol), owner(), access())
          assert can_download(User.to_jid(shared.robert), owner(), access())
          assert can_download(User.to_jid(shared.karen), owner(), access())
          assert can_download(User.to_jid(shared.tim), owner(), access())
        end
      end
    end
  end
end
