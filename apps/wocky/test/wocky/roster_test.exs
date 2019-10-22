defmodule Wocky.RosterTest do
  use Wocky.DataCase, async: true

  alias Faker.Code
  alias Faker.Name
  alias Wocky.Block
  alias Wocky.Notifier.Push
  alias Wocky.Notifier.Push.Backend.Sandbox
  alias Wocky.Relation
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Roster
  alias Wocky.Roster.Item

  setup do
    # A user with 5 friends
    user = Factory.insert(:user)

    friends =
      for _ <- 1..5 do
        c = Factory.insert(:user)
        Roster.befriend(user, c)
        c
      end

    rosterless_user = Factory.insert(:user)

    inviter = Factory.insert(:user)
    Factory.insert(:user_invitation, user: inviter, invitee: user)

    invitee = Factory.insert(:user)
    Factory.insert(:user_invitation, user: user, invitee: invitee)

    blocked_viewer = Factory.insert(:user)
    Block.block(inviter, blocked_viewer)
    Block.block(blocked_viewer, invitee)

    nil_handle_user = Factory.insert(:user, handle: nil)
    Roster.befriend(nil_handle_user, user)

    {:ok,
     user: user,
     all_friends: Enum.sort([nil_handle_user | friends]),
     contact: hd(friends),
     rosterless_user: rosterless_user,
     inviter: inviter,
     invitee: invitee,
     blocked_viewer: blocked_viewer}
  end

  describe "get_item/2" do
    test "should return the roster item for the specified contact", ctx do
      Enum.map(ctx.all_friends, fn c ->
        assert ctx.user |> Roster.get_item(c) |> Map.get(:contact_id) == c.id
      end)
    end
  end

  describe "update/2" do
    test "should update the existing contact", ctx do
      new_name = Name.first_name()
      item = Roster.get_item(ctx.user, ctx.contact)

      assert {:ok, %Item{}} = Roster.update_item(item, %{name: new_name})

      new_item = Roster.get_item(ctx.user, ctx.contact)
      assert new_item.contact_id == ctx.contact.id
      assert new_item.name == new_name
    end
  end

  describe "self_or_friend?/1" do
    test "should return true when a user is a friend", ctx do
      assert Roster.self_or_friend?(ctx.user, ctx.contact)
    end

    test "should return false if the user has blocked the contact", ctx do
      Block.block(ctx.user, ctx.contact)

      refute Roster.self_or_friend?(ctx.user, ctx.contact)
    end

    test "should return false if the contact has blocked the user", ctx do
      Block.block(ctx.contact, ctx.user)

      refute Roster.self_or_friend?(ctx.user, ctx.contact)
    end

    test "should return false for non-existant friends", ctx do
      refute Roster.self_or_friend?(ctx.user, Factory.build(:user))
      refute Roster.self_or_friend?(ctx.user, ctx.rosterless_user)
    end

    test "should return true for self", ctx do
      assert Roster.self_or_friend?(ctx.user, ctx.user)
    end
  end

  describe "friend?/1" do
    test "should return true when a user is a friend", ctx do
      assert Roster.friend?(ctx.user, ctx.contact)
    end

    test "should return false if the user has blocked the contact", ctx do
      Block.block(ctx.user, ctx.contact)

      refute Roster.friend?(ctx.user, ctx.contact)
    end

    test "should return false if the contact has blocked the user", ctx do
      Block.block(ctx.contact, ctx.user)

      refute Roster.friend?(ctx.user, ctx.contact)
    end

    test "should return false for non-existant friends", ctx do
      refute Roster.friend?(ctx.user, Factory.build(:user))
      refute Roster.friend?(ctx.user, ctx.rosterless_user)
    end

    test "should return false for self", ctx do
      refute Roster.friend?(ctx.user, ctx.user)
    end
  end

  describe "friends_query/2" do
    setup ctx do
      blocked_user = Factory.insert(:user, %{name: "BLOCKYMCBLOCK"})
      Block.block(blocked_user, ctx.blocked_viewer)
      {:ok, blocked_user: blocked_user}
    end

    test "should return all unblocked friends", ctx do
      query = Roster.friends_query(ctx.user, ctx.user)

      assert Enum.sort(Repo.all(query)) == Enum.sort(ctx.all_friends)
    end

    test "should not return entries blocked by the requester", ctx do
      assert {:error, :permission_denied} ==
               Roster.friends_query(ctx.user, ctx.blocked_user)
    end
  end

  describe "relationship/2" do
    test "should return :self when both user IDs are equal", ctx do
      assert Roster.relationship(ctx.user, ctx.user) == :self
    end

    test "should return :friend where the two users are friends", ctx do
      assert Roster.relationship(ctx.user, ctx.contact) == :friend
      assert Roster.relationship(ctx.contact, ctx.user) == :friend
    end

    test "should return :invited where user a has invited user b", ctx do
      assert Roster.relationship(ctx.user, ctx.invitee) == :invited
    end

    test "should return :invited_by where user b has invited user a", ctx do
      assert Roster.relationship(ctx.user, ctx.inviter) == :invited_by
    end

    test "should return :none if the users have no relationship", ctx do
      assert Roster.relationship(ctx.user, ctx.rosterless_user) == :none
      assert Roster.relationship(ctx.rosterless_user, ctx.user) == :none
    end
  end

  describe "items_query/2" do
    test "should return all roster items for a user", ctx do
      assert ctx.user
             |> Roster.items_query(ctx.user)
             |> Repo.all()
             |> Enum.map(& &1.contact_id)
             |> Enum.sort() ==
               ctx.all_friends |> Enum.map(& &1.id) |> Enum.sort()
    end

    test "should return an error for a non-self user", ctx do
      assert Roster.items_query(ctx.user, ctx.contact) ==
               {:error, :permission_denied}
    end
  end

  describe "relationship management functions" do
    setup do
      user2 = Factory.insert(:user)
      {:ok, user2: user2}
    end

    test "befriend/2 when there is no existing relationship", ctx do
      assert :ok = Roster.befriend(ctx.user, ctx.user2)
      assert Roster.friend?(ctx.user, ctx.user2)
    end

    test "befriend/2 when there is an existing relationship", ctx do
      name = Name.first_name()
      name2 = Name.first_name()

      Factory.insert(
        :roster_item,
        name: name,
        user: ctx.user,
        contact: ctx.user2,
        name: name
      )

      Factory.insert(
        :roster_item,
        user: ctx.user2,
        contact: ctx.user,
        name: name2
      )

      assert :ok = Roster.befriend(ctx.user, ctx.user2)
      assert Roster.friend?(ctx.user, ctx.user2)
      assert Roster.get_item(ctx.user, ctx.user2).name == name
      assert Roster.get_item(ctx.user2, ctx.user).name == name2
    end

    test "should generate a push notification", ctx do
      Sandbox.clear_notifications()
      Push.enable(ctx.user2, "testing", Code.isbn13())

      assert :ok = Roster.befriend(ctx.user, ctx.user2)

      notifications = Sandbox.wait_notifications(count: 1, timeout: 5000)
      assert Enum.count(notifications) == 1
    end

    test "unfriend/2 when users are friends", ctx do
      Roster.befriend(ctx.user, ctx.user2)

      assert :ok = Roster.unfriend(ctx.user, ctx.user2)
      assert Roster.relationship(ctx.user, ctx.user2) == :none
    end

    test "unfriend/2 when there is no existing relationship", ctx do
      assert :ok = Roster.unfriend(ctx.user, ctx.user2)
      assert Roster.relationship(ctx.user, ctx.user2) == :none
    end
  end

  describe "unfriend cleanup" do
    setup ctx do
      user_bot = Factory.insert(:bot, user: ctx.user)
      contact_bot = Factory.insert(:bot, user: ctx.contact)

      {:ok, user_bot: user_bot, contact_bot: contact_bot}
    end

    test "bots should no longer be subscribed", ctx do
      Relation.subscribe(ctx.contact, ctx.user_bot)
      Relation.subscribe(ctx.user, ctx.contact_bot)

      Roster.unfriend(ctx.user, ctx.contact)

      refute Relation.subscribed?(ctx.contact, ctx.user_bot)
      refute Relation.subscribed?(ctx.user, ctx.contact_bot)
    end

    test "bot invitations should be removed", ctx do
      Relation.invite(ctx.contact, ctx.user_bot, ctx.user)
      Relation.invite(ctx.user, ctx.contact_bot, ctx.contact)

      Roster.unfriend(ctx.user, ctx.contact)

      refute Relation.invited?(ctx.contact, ctx.user_bot)
      refute Relation.invited?(ctx.user, ctx.contact_bot)
    end

    test "locations shares should be canceled", ctx do
      Roster.start_sharing_location(ctx.user, ctx.contact)

      Roster.unfriend(ctx.user, ctx.contact)

      assert Roster.get_location_shares(ctx.user) == []
    end
  end

  describe "sent_invitations_query/1" do
    test "return a query that will list all invited users", ctx do
      assert ctx.user
             |> Roster.sent_invitations_query(ctx.user)
             |> Repo.one!()
             |> Map.get(:invitee_id) == ctx.invitee.id
    end

    test "will fail when it's not made on self", ctx do
      assert Roster.sent_invitations_query(ctx.user, ctx.invitee) ==
               {:error, :permission_denied}
    end
  end

  describe "received_invitations_query/1" do
    test "return a query that will list all users that sent an invitation",
         ctx do
      assert ctx.user
             |> Roster.received_invitations_query(ctx.user)
             |> Repo.one!()
             |> Map.get(:user_id) == ctx.inviter.id
    end

    test "will fail when it's not made on self", ctx do
      assert Roster.received_invitations_query(ctx.user, ctx.invitee) ==
               {:error, :permission_denied}
    end
  end

  describe "invite/2" do
    setup do
      {:ok, stranger: Factory.insert(:user)}
    end

    test "should create an invitation for a stranger", ctx do
      assert Roster.relationship(ctx.user, ctx.stranger) == :none

      assert Roster.invite(ctx.user, ctx.stranger) == :invited

      assert Roster.relationship(ctx.user, ctx.stranger) == :invited
    end

    test "should accept an existing invitation", ctx do
      assert Roster.invite(ctx.user, ctx.inviter) == :friend

      assert Roster.relationship(ctx.user, ctx.inviter) == :friend
    end

    test "should have no additional effect after multiple calls", ctx do
      assert Roster.invite(ctx.user, ctx.stranger) == :invited
      assert Roster.invite(ctx.user, ctx.stranger) == :invited
      assert Roster.relationship(ctx.user, ctx.stranger) == :invited
    end

    test "should no no effect on an existing friend", ctx do
      assert Roster.invite(ctx.user, ctx.contact) == :friend
      assert Roster.relationship(ctx.user, ctx.contact) == :friend
    end

    test "should have no effect on self", ctx do
      assert Roster.invite(ctx.user, ctx.user) == :self
      assert Roster.relationship(ctx.user, ctx.user) == :self
    end
  end

  describe "start_sharing_location/3" do
    test "should create a share record", ctx do
      assert {:ok, _} = Roster.start_sharing_location(ctx.user, ctx.contact)

      assert [%Item{} = share] = Roster.get_location_shares(ctx.user)
      assert [%Item{} = ^share] = Roster.get_location_sharers(ctx.contact)
      assert share.contact_id == ctx.contact.id
      assert share.share_type == :always
    end

    test "should update an existing share record", ctx do
      Roster.start_sharing_location(ctx.user, ctx.contact)

      assert {:ok, _} =
               Roster.start_sharing_location(ctx.user, ctx.contact, :nearby)

      assert [%Item{} = share] = Roster.get_location_shares(ctx.user)
      assert share.share_type == :nearby
    end

    test "should not share location with a stranger", ctx do
      stranger = Factory.insert(:user)

      assert {:error, _} = Roster.start_sharing_location(ctx.user, stranger)
      assert Roster.get_location_shares(ctx.user) == []
    end

    test "should not share with self", ctx do
      assert {:error, _} = Roster.start_sharing_location(ctx.user, ctx.user)
      assert Roster.get_location_shares(ctx.user) == []
    end
  end

  describe "stop_sharing_location/2" do
    test "should remove existing location share", ctx do
      Roster.start_sharing_location(ctx.user, ctx.contact)

      assert :ok = Roster.stop_sharing_location(ctx.user, ctx.contact)
      assert Roster.get_location_shares(ctx.user) == []
    end

    test "should succeed if no location share exists", ctx do
      stranger = Factory.insert(:user)

      assert :ok = Roster.stop_sharing_location(ctx.user, stranger)
    end
  end

  describe "stop_sharing_location/1" do
    test "should remove existing location share", ctx do
      Roster.start_sharing_location(ctx.user, ctx.contact)

      assert :ok = Roster.stop_sharing_location(ctx.user)
      assert Roster.get_location_shares(ctx.user) == []
    end

    test "should succeed if no location share exists", ctx do
      assert :ok = Roster.stop_sharing_location(ctx.user)
    end
  end

  describe "get_location_shares/1" do
    test "should not return disabled location shares", ctx do
      # Sharing is disabled by default on new friendships
      assert Roster.get_location_shares(ctx.user) == []
    end
  end

  describe "get_location_sharers/1" do
    test "should not return disabled location shares", ctx do
      # Sharing is disabled by default on new friendships
      assert Roster.get_location_sharers(ctx.contact) == []
    end
  end
end
