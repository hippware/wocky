defmodule Wocky.RosterTest do
  use Wocky.DataCase, async: true

  alias Faker.Name
  alias Wocky.Block
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Roster
  alias Wocky.Roster.Item, as: RosterItem

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

  describe "get/2" do
    test "should return the roster item for the specified contact", ctx do
      Enum.map(ctx.all_friends, fn c ->
        assert ctx.user |> Roster.get_item(c) |> Map.get(:contact_id) == c.id
      end)
    end
  end

  describe "set_name/3" do
    test "should update the existing contact", ctx do
      new_name = Name.first_name()

      assert {:ok, %RosterItem{}} =
               Roster.set_name(ctx.user, ctx.contact, new_name)

      item = Roster.get_item(ctx.user, ctx.contact)
      assert item.contact_id == ctx.contact.id
      assert item.name == new_name
    end

    test "should fail on a non-existant user", ctx do
      other = Factory.build(:user)
      assert {:error, _} = Roster.set_name(ctx.user, other, Name.first_name())

      assert nil == Roster.get_item(ctx.user, other)
    end

    test "should fail for a non-friend user", ctx do
      assert {:error, _} =
               Roster.set_name(ctx.user, ctx.invitee, Name.first_name())

      assert nil == Roster.get_item(ctx.user, ctx.invitee)
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
        user_id: ctx.user.id,
        contact_id: ctx.user2.id,
        name: name
      )

      Factory.insert(
        :roster_item,
        user_id: ctx.user2.id,
        contact_id: ctx.user.id,
        name: name2
      )

      assert :ok = Roster.befriend(ctx.user, ctx.user2)
      assert Roster.friend?(ctx.user, ctx.user2)
      assert Roster.get_item(ctx.user, ctx.user2).name == name
      assert Roster.get_item(ctx.user2, ctx.user).name == name2
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
end
