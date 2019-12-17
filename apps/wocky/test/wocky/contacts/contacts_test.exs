defmodule Wocky.Contacts.ContactsTest do
  use Wocky.DataCase, async: true

  alias Faker.Code
  alias Wocky.Contacts
  alias Wocky.Contacts.Relationship
  alias Wocky.Notifier.InBand.Notification
  alias Wocky.Notifier.Push
  alias Wocky.Notifier.Push.Backend.Sandbox
  alias Wocky.Repo
  alias Wocky.Repo.Factory

  setup do
    {:ok, user: Factory.insert(:user), contact: Factory.insert(:user)}
  end

  # ----------------------------------------------------------------------
  # Relationship management

  describe "befriend/3" do
    setup ctx do
      result = Contacts.befriend(ctx.user, ctx.contact, :always)

      {:ok, result: result}
    end

    test "should return :ok", ctx do
      assert ctx.result == :ok
    end

    test "should make two users friends", ctx do
      assert Contacts.friend?(ctx.user, ctx.contact)
    end

    test "should set sharing for both users", ctx do
      assert Contacts.share_type(ctx.user, ctx.contact) == :always
      assert Contacts.share_type(ctx.contact, ctx.user) == :always
    end

    test "should update sharing when friendship already exists", ctx do
      assert :ok = Contacts.befriend(ctx.user, ctx.contact, :nearby)

      assert Contacts.share_type(ctx.user, ctx.contact) == :nearby
      assert Contacts.share_type(ctx.contact, ctx.user) == :nearby
    end
  end

  describe "make_friends/3" do
    test "should return an error when friending self", ctx do
      assert {:error, cs} = Contacts.make_friends(ctx.user, ctx.user, :always)
      assert errors_on(cs).contact_id == ["self"]
    end
  end

  describe "make_friends/3 when no relationship exists" do
    setup ctx do
      result = Contacts.make_friends(ctx.user, ctx.contact, :always)

      {:ok, result: result}
    end

    test "should return {:ok, :invited}", ctx do
      assert ctx.result == {:ok, :invited}
    end

    test "should create an invitation", ctx do
      assert Contacts.invited?(ctx.user, ctx.contact)
      assert Contacts.invited_by?(ctx.contact, ctx.user)
    end

    test "should store the inviter's sharing preference", ctx do
      assert Contacts.share_type(ctx.user, ctx.contact) == :always
    end

    test "should update sharing when inviter resubmits invitation", ctx do
      assert Contacts.make_friends(ctx.user, ctx.contact, :nearby) ==
               {:ok, :invited}

      assert Contacts.share_type(ctx.user, ctx.contact) == :nearby
    end
  end

  describe "make_friends/3 notifications when no relationship exists" do
    setup ctx do
      Sandbox.clear_notifications()

      token = Code.isbn13()
      Push.enable(ctx.contact, "testing", token)

      {:ok, :invited} = Contacts.make_friends(ctx.user, ctx.contact, :always)

      {:ok, token: token}
    end

    test "should generate an in-band notification to the invitee", ctx do
      assert %Notification{type: :user_invitation} =
               Repo.get_by(Notification, user_id: ctx.contact.id)
    end

    test "should generate a push notification to the invitee", ctx do
      assert [notification] =
               Sandbox.wait_notifications(count: 1, timeout: 5000)

      assert notification.device_token == ctx.token
      assert notification.payload["aps"]["alert"] =~ ctx.user.handle
      assert notification.payload["aps"]["alert"] =~ "wants to connect"
    end
  end

  describe "make_friends/3 when an invitation already exists" do
    setup ctx do
      {:ok, :invited} = Contacts.make_friends(ctx.user, ctx.contact, :always)

      result = Contacts.make_friends(ctx.contact, ctx.user, :nearby)

      {:ok, result: result}
    end

    test "should return {:ok, :friend}", ctx do
      assert ctx.result == {:ok, :friend}
    end

    test "should set the users as friends", ctx do
      assert Contacts.friend?(ctx.user, ctx.contact)
    end

    test "should set the invitee's sharing preference", ctx do
      assert Contacts.share_type(ctx.contact, ctx.user) == :nearby
    end

    test "should preserve the inviter's sharing preference", ctx do
      assert Contacts.share_type(ctx.user, ctx.contact) == :always
    end

    test "should have no effect on an existing friend", ctx do
      assert Contacts.make_friends(ctx.contact, ctx.user, :always) ==
               {:ok, :friend}

      assert {:friend, user_rel, contact_rel} =
               Contacts.get_relationship(ctx.user, ctx.contact)

      assert user_rel.state == :friend
      assert user_rel.share_type == :always

      assert contact_rel.state == :friend
      assert contact_rel.share_type == :nearby
    end
  end

  describe "make_friends/3 notifications when an invitation already exists" do
    setup ctx do
      Sandbox.clear_notifications()

      token = Code.isbn13()
      Push.enable(ctx.user, "testing", token)

      {:ok, :invited} = Contacts.make_friends(ctx.user, ctx.contact, :always)
      {:ok, :friend} = Contacts.make_friends(ctx.contact, ctx.user, :always)

      {:ok, token: token}
    end

    test "should not generate an in-band notification to the inviter", ctx do
      refute Repo.get_by(Notification, user_id: ctx.user.id)
    end

    test "should generate a push notification to the inviter", ctx do
      assert [notification] =
               Sandbox.wait_notifications(count: 1, timeout: 5000)

      assert notification.device_token == ctx.token
      assert notification.payload["aps"]["alert"] =~ ctx.contact.handle
      assert notification.payload["aps"]["alert"] =~ "connected with you"
    end
  end

  describe "make_friends/3 when a blocking relationship exists" do
    setup ctx do
      :ok = Contacts.block(ctx.user, ctx.contact)
    end

    test "should not allow user to invite blockee", ctx do
      assert {:error, cs} =
               Contacts.make_friends(ctx.user, ctx.contact, :always)

      assert errors_on(cs).contact_id == ["blocked"]
    end

    test "should not allow user to invite blocker", ctx do
      assert {:error, cs} =
               Contacts.make_friends(ctx.contact, ctx.user, :always)

      assert errors_on(cs).contact_id == ["blocked"]
    end
  end

  describe "unfriend/2 when no relationship exists" do
    setup ctx do
      result = Contacts.unfriend(ctx.user, ctx.contact)

      {:ok, result: result}
    end

    test "should return :ok", ctx do
      assert ctx.result == :ok
    end

    test "should not create a relationship", ctx do
      assert {:none, nil, nil} =
               Contacts.get_relationship(ctx.user, ctx.contact)
    end
  end

  describe "unfriend/2 when an invitation exists" do
    setup ctx do
      {:ok, :invited} = Contacts.make_friends(ctx.user, ctx.contact, :always)

      result = Contacts.unfriend(ctx.user, ctx.contact)

      {:ok, result: result}
    end

    test "should return :ok", ctx do
      assert ctx.result == :ok
    end

    test "should remove the invitation", ctx do
      refute Contacts.invited?(ctx.user, ctx.contact)
      refute Contacts.invited_by?(ctx.contact, ctx.user)
    end

    test "should leave no relationship", ctx do
      assert {:none, nil, nil} =
               Contacts.get_relationship(ctx.user, ctx.contact)
    end
  end

  describe "unfriend/2 when a friendship exists" do
    setup ctx do
      Contacts.befriend(ctx.user, ctx.contact, :always)

      result = Contacts.unfriend(ctx.user, ctx.contact)

      {:ok, result: result}
    end

    test "should return :ok", ctx do
      assert ctx.result == :ok
    end

    test "should remove the friendship", ctx do
      refute Contacts.friend?(ctx.user, ctx.contact)
      refute Contacts.friend?(ctx.contact, ctx.user)
    end

    test "should leave no relationship", ctx do
      assert {:none, nil, nil} =
               Contacts.get_relationship(ctx.user, ctx.contact)
    end

    test "should disable location sharing", ctx do
      assert Contacts.share_type(ctx.user, ctx.contact) == :disabled
      assert Contacts.share_type(ctx.contact, ctx.user) == :disabled
    end

    test "should remove all location shares", ctx do
      assert Contacts.get_location_shares(ctx.user) == []
      assert Contacts.get_location_shares(ctx.contact) == []
    end
  end

  describe "unfriend/2 when a blocking relationship exists" do
    setup ctx do
      :ok = Contacts.block(ctx.user, ctx.contact)
    end

    test "should not remove a blocker", ctx do
      assert :ok = Contacts.unfriend(ctx.contact, ctx.user)
      assert Contacts.relationship(ctx.contact, ctx.user) == :blocked_by
    end

    test "should not remove a blockee", ctx do
      assert :ok = Contacts.unfriend(ctx.user, ctx.contact)
      assert Contacts.relationship(ctx.user, ctx.contact) == :blocked
    end
  end

  describe "block/2 when no relationship exists" do
    setup ctx do
      result = Contacts.block(ctx.user, ctx.contact)

      {:ok, result: result}
    end

    test "should return :ok", ctx do
      assert :ok = ctx.result
    end

    test "should block the other user", ctx do
      assert Contacts.relationship(ctx.user, ctx.contact) == :blocked
      assert Contacts.relationship(ctx.contact, ctx.user) == :blocked_by
    end

    test "should disable location sharing", ctx do
      assert Contacts.share_type(ctx.user, ctx.contact) == :disabled
    end

    test "should fail if the contact doesn't exist", ctx do
      bogus = Factory.build(:user)

      assert {:error, _} = Contacts.block(ctx.user, bogus)

      refute Contacts.blocked?(ctx.user, bogus)
    end
  end

  describe "block/2 when a non-blocking relationship exists" do
    setup ctx do
      Contacts.befriend(ctx.user, ctx.contact, :always)

      result = Contacts.block(ctx.user, ctx.contact)

      {:ok, result: result}
    end

    test "should return :ok", ctx do
      assert :ok = ctx.result
    end

    test "should block the other user", ctx do
      assert Contacts.relationship(ctx.user, ctx.contact) == :blocked
      assert Contacts.relationship(ctx.contact, ctx.user) == :blocked_by
    end

    test "should delete the other side of the relationship", ctx do
      assert {:blocked_by, nil, _} =
               Contacts.get_relationship(ctx.contact, ctx.user)
    end

    test "should disable location sharing", ctx do
      assert Contacts.share_type(ctx.user, ctx.contact) == :disabled
      assert Contacts.share_type(ctx.contact, ctx.user) == :disabled
    end

    test "should remove all location shares", ctx do
      assert Contacts.get_location_shares(ctx.user) == []
      assert Contacts.get_location_shares(ctx.contact) == []
    end
  end

  describe "block/2 when the user has already blocked the contact" do
    test "should return :ok", ctx do
      assert :ok = Contacts.block(ctx.user, ctx.contact)
      assert :ok = Contacts.block(ctx.user, ctx.contact)
    end
  end

  describe "block/2 when the user has been blocked by the contact" do
    setup ctx do
      assert :ok = Contacts.block(ctx.contact, ctx.user)

      result = Contacts.block(ctx.user, ctx.contact)

      {:ok, result: result}
    end

    test "should return :ok", ctx do
      assert :ok = ctx.result
    end

    test "should set both users as blocking the other", ctx do
      assert Contacts.relationship(ctx.user, ctx.contact) == :blocked
      assert Contacts.relationship(ctx.contact, ctx.user) == :blocked
    end
  end

  describe "unblock/2 when no relationship exists" do
    setup ctx do
      result = Contacts.unblock(ctx.user, ctx.contact)

      {:ok, result: result}
    end

    test "should return :ok", ctx do
      assert ctx.result == :ok
    end

    test "should not create a relationship", ctx do
      assert {:none, nil, nil} =
               Contacts.get_relationship(ctx.user, ctx.contact)
    end
  end

  describe "unblock/2 when a non-blocking relationship exists" do
    setup ctx do
      Contacts.befriend(ctx.user, ctx.contact)

      result = Contacts.unblock(ctx.user, ctx.contact)

      {:ok, result: result}
    end

    test "should return :ok", ctx do
      assert ctx.result == :ok
    end

    test "should not change the relationship", ctx do
      assert Contacts.friend?(ctx.user, ctx.contact)
      assert Contacts.friend?(ctx.contact, ctx.user)
    end
  end

  describe "unblock/2 when the user has blocked the contact" do
    setup ctx do
      :ok = Contacts.block(ctx.user, ctx.contact)

      result = Contacts.unblock(ctx.user, ctx.contact)

      {:ok, result: result}
    end

    test "should return :ok", ctx do
      assert ctx.result == :ok
    end

    test "should remove the block", ctx do
      refute Contacts.blocked?(ctx.user, ctx.contact)
    end

    test "should leave no relationship", ctx do
      assert {:none, nil, nil} =
               Contacts.get_relationship(ctx.user, ctx.contact)
    end
  end

  describe "unblock/2 when the user has been blocked by the contact" do
    setup ctx do
      :ok = Contacts.block(ctx.contact, ctx.user)

      result = Contacts.unblock(ctx.user, ctx.contact)

      {:ok, result: result}
    end

    test "should return :ok", ctx do
      assert ctx.result == :ok
    end

    test "should not remove the block", ctx do
      assert Contacts.blocked?(ctx.user, ctx.contact)
    end

    test "should leave the user in a blocked_by state", ctx do
      assert Contacts.relationship(ctx.user, ctx.contact) == :blocked_by
    end
  end

  describe "unblock/2 when the users have blocked each other" do
    setup ctx do
      :ok = Contacts.block(ctx.user, ctx.contact)
      :ok = Contacts.block(ctx.contact, ctx.user)

      result = Contacts.unblock(ctx.user, ctx.contact)

      {:ok, result: result}
    end

    test "should return :ok", ctx do
      assert ctx.result == :ok
    end

    test "should not remove the block", ctx do
      assert Contacts.blocked?(ctx.user, ctx.contact)
    end

    test "should leave the user in a blocked_by state", ctx do
      assert Contacts.relationship(ctx.user, ctx.contact) == :blocked_by
    end
  end

  describe "update_sharing/4" do
    setup ctx do
      Contacts.befriend(ctx.user, ctx.contact)

      :ok
    end

    test "should start sharing", ctx do
      assert {:ok, _} = Contacts.update_sharing(ctx.user, ctx.contact, :always)

      assert [%Relationship{} = share] = Contacts.get_location_shares(ctx.user)

      assert [%Relationship{} = ^share] =
               Contacts.get_location_sharers(ctx.contact)

      assert share.contact_id == ctx.contact.id
      assert share.share_type == :always
    end

    test "should update share type when sharing is already enabled", ctx do
      assert {:ok, _} = Contacts.update_sharing(ctx.user, ctx.contact, :always)
      assert {:ok, _} = Contacts.update_sharing(ctx.user, ctx.contact, :nearby)

      assert [%Relationship{} = share] = Contacts.get_location_shares(ctx.user)
      assert share.share_type == :nearby
    end

    test "should disable sharing", ctx do
      assert {:ok, _} = Contacts.update_sharing(ctx.user, ctx.contact, :always)

      assert {:ok, _} =
               Contacts.update_sharing(ctx.user, ctx.contact, :disabled)

      assert Contacts.get_location_shares(ctx.user) == []
    end

    test "should set distance and cooldown", ctx do
      dist = Faker.random_between(500, 1000)
      cooldown = Faker.random_between(1, 100)

      assert {:ok, _} =
               Contacts.update_sharing(ctx.user, ctx.contact, :always,
                 nearby_distance: dist,
                 nearby_cooldown: cooldown
               )

      assert [%Relationship{nearby_distance: ^dist, nearby_cooldown: ^cooldown}] =
               Contacts.get_location_shares(ctx.user)
    end

    test "should not share location with a stranger", ctx do
      stranger = Factory.insert(:user)

      assert {:error, cs} = Contacts.update_sharing(ctx.user, stranger, :always)
      assert errors_on(cs).contact_id == ["must be a friend"]
      assert Contacts.get_location_shares(ctx.user) == []
    end

    test "should fail with bad share type", ctx do
      assert {:error, cs} = Contacts.update_sharing(ctx.user, ctx.contact, :bad)
      assert errors_on(cs).share_type == ["is invalid"]
    end

    test "should not share with self", ctx do
      assert {:error, cs} = Contacts.update_sharing(ctx.user, ctx.user, :always)
      assert errors_on(cs).contact_id == ["must be a friend"]
      assert Contacts.get_location_shares(ctx.user) == []
    end
  end

  describe "stop_sharing_location/1" do
    setup ctx do
      Contacts.befriend(ctx.user, ctx.contact)

      :ok
    end

    test "should remove existing location share", ctx do
      assert {:ok, _} = Contacts.update_sharing(ctx.user, ctx.contact, :always)
      assert :ok = Contacts.stop_sharing_location(ctx.user)
      assert Contacts.get_location_shares(ctx.user) == []
    end

    test "should succeed if no location share exists", ctx do
      assert :ok = Contacts.stop_sharing_location(ctx.user)
    end
  end

  # ----------------------------------------------------------------------
  # Relationship status

  describe "relationship/2" do
    test "should return :none when the users are not related", ctx do
      assert Contacts.relationship(ctx.user, ctx.contact) == :none
      assert Contacts.relationship(ctx.contact, ctx.user) == :none
    end

    test "should return :self when both user IDs are equal", ctx do
      assert Contacts.relationship(ctx.user, ctx.user) == :self
    end

    test "should return :friend when the users are friends", ctx do
      Contacts.befriend(ctx.user, ctx.contact)

      assert Contacts.relationship(ctx.user, ctx.contact) == :friend
      assert Contacts.relationship(ctx.contact, ctx.user) == :friend
    end

    test "should return :invited when user a has invited user b", ctx do
      Contacts.make_friends(ctx.user, ctx.contact, :always)

      assert Contacts.relationship(ctx.user, ctx.contact) == :invited
    end

    test "should return :invited_by when user b has invited user a", ctx do
      Contacts.make_friends(ctx.contact, ctx.user, :always)

      assert Contacts.relationship(ctx.user, ctx.contact) == :invited_by
    end

    test "should return :blocked when user a has blocked user b", ctx do
      Contacts.block(ctx.user, ctx.contact)

      assert Contacts.relationship(ctx.user, ctx.contact) == :blocked
    end

    test "should return :blocked_by when user b has blocked user a", ctx do
      Contacts.block(ctx.contact, ctx.user)

      assert Contacts.relationship(ctx.user, ctx.contact) == :blocked_by
    end
  end

  describe "share_type/2" do
    test "should return the share when type users have a relationship", ctx do
      Contacts.make_friends(ctx.user, ctx.contact, :always)
      Contacts.make_friends(ctx.contact, ctx.user, :nearby)

      assert Contacts.share_type(ctx.user, ctx.contact) == :always
      assert Contacts.share_type(ctx.contact, ctx.user) == :nearby
    end

    test "should return :disabled when no relationship exists", ctx do
      assert Contacts.share_type(ctx.user, ctx.contact) == :disabled
    end
  end

  describe "get_relationship/2" do
    test "should return {:none, nil, nil} when there is no relationship", ctx do
      assert {:none, nil, nil} =
               Contacts.get_relationship(ctx.user, ctx.contact)

      assert {:none, nil, nil} =
               Contacts.get_relationship(ctx.contact, ctx.user)
    end

    test "should return {:self, nil, nil} when both user IDs are equal", ctx do
      assert {:self, nil, nil} = Contacts.get_relationship(ctx.user, ctx.user)
    end

    test "should return {:friend, _, _} when the users are friends", ctx do
      Contacts.befriend(ctx.user, ctx.contact)

      assert {:friend, rel1, rel2} =
               Contacts.get_relationship(ctx.user, ctx.contact)

      assert {:friend, ^rel2, ^rel1} =
               Contacts.get_relationship(ctx.contact, ctx.user)
    end

    test "should return {:invited, _, nil} when a user has invited", ctx do
      Contacts.make_friends(ctx.user, ctx.contact, :always)

      assert {:invited, _, nil} =
               Contacts.get_relationship(ctx.user, ctx.contact)
    end

    test "should return {:invited_by, nil, _} when user is invited by", ctx do
      Contacts.make_friends(ctx.contact, ctx.user, :always)

      assert {:invited_by, nil, _} =
               Contacts.get_relationship(ctx.user, ctx.contact)
    end

    test "should return {:blocked, _, nil} when user has blocked", ctx do
      Contacts.block(ctx.user, ctx.contact)

      assert {:blocked, _, nil} =
               Contacts.get_relationship(ctx.user, ctx.contact)
    end

    test "should return {:blocked_by, nil, _} when user is blocked", ctx do
      Contacts.block(ctx.contact, ctx.user)

      assert {:blocked_by, nil, _} =
               Contacts.get_relationship(ctx.user, ctx.contact)
    end
  end

  describe "self?/2" do
    test "should return true for self", ctx do
      assert Contacts.self?(ctx.user, ctx.user)
    end

    test "should return false for other users", ctx do
      refute Contacts.self?(ctx.user, ctx.contact)
    end
  end

  describe "friend?/2" do
    test "should return true when a user is a friend", ctx do
      Contacts.befriend(ctx.user, ctx.contact)

      assert Contacts.friend?(ctx.user, ctx.contact)
    end

    test "should return false if the user has blocked the contact", ctx do
      Contacts.block(ctx.user, ctx.contact)

      refute Contacts.friend?(ctx.user, ctx.contact)
    end

    test "should return false if the contact has blocked the user", ctx do
      Contacts.block(ctx.contact, ctx.user)

      refute Contacts.friend?(ctx.user, ctx.contact)
    end

    test "should return false for non-existant friends", ctx do
      refute Contacts.friend?(ctx.user, Factory.build(:user))
      refute Contacts.friend?(ctx.user, ctx.contact)
    end

    test "should return false for self", ctx do
      refute Contacts.friend?(ctx.user, ctx.user)
    end
  end

  describe "invited?/2 and invited_by?/2" do
    setup ctx do
      {:ok, :invited} = Contacts.make_friends(ctx.user, ctx.contact, :always)

      :ok
    end

    test "should return true when the user has invited the contact", ctx do
      assert Contacts.invited?(ctx.user, ctx.contact)
      assert Contacts.invited_by?(ctx.contact, ctx.user)
    end

    test "should return false when no invitation exists", ctx do
      refute Contacts.invited?(ctx.contact, ctx.user)
      refute Contacts.invited_by?(ctx.user, ctx.contact)
    end
  end

  describe "blocked/2" do
    test "should return true when either user has blocked the other", ctx do
      :ok = Contacts.block(ctx.user, ctx.contact)

      assert Contacts.blocked?(ctx.user, ctx.contact)
      assert Contacts.blocked?(ctx.contact, ctx.user)
    end

    test "should return false when no blocking relationship exists", ctx do
      refute Contacts.blocked?(ctx.user, ctx.contact)
      refute Contacts.blocked?(ctx.contact, ctx.user)
    end
  end

  # ----------------------------------------------------------------------
  # Queries

  describe "sent_invitations_query/1" do
    test "return a query that will list all invited users", ctx do
      Contacts.make_friends(ctx.user, ctx.contact, :always)

      assert ctx.user
             |> Contacts.sent_invitations_query(ctx.user)
             |> Repo.one!()
             |> Map.get(:contact_id) == ctx.contact.id
    end

    test "will fail when it's not made on self", ctx do
      assert Contacts.sent_invitations_query(ctx.user, ctx.contact) ==
               {:error, :permission_denied}
    end
  end

  describe "received_invitations_query/1" do
    test "return a query that will list all users that sent an invitation",
         ctx do
      Contacts.make_friends(ctx.contact, ctx.user, :always)

      assert ctx.user
             |> Contacts.received_invitations_query(ctx.user)
             |> Repo.one!()
             |> Map.get(:user_id) == ctx.contact.id
    end

    test "will fail when it's not made on self", ctx do
      assert Contacts.received_invitations_query(ctx.user, ctx.contact) ==
               {:error, :permission_denied}
    end
  end

  defp setup_friends(ctx) do
    friends =
      for _ <- 1..5 do
        c = Factory.insert(:user)
        Contacts.befriend(ctx.user, c)
        c
      end

    {:ok, all_friends: Enum.sort(friends)}
  end

  describe "friends_query/2" do
    setup :setup_friends

    test "should return all unblocked friends", ctx do
      query = Contacts.friends_query(ctx.user, ctx.user)

      assert Enum.sort(Repo.all(query)) == Enum.sort(ctx.all_friends)
    end

    test "should not return entries blocked by the requester", ctx do
      assert {:error, :permission_denied} ==
               Contacts.friends_query(ctx.user, ctx.contact)
    end
  end

  describe "friend_relationships_query/2" do
    setup :setup_friends

    test "should return all roster items for a user", ctx do
      assert ctx.user
             |> Contacts.friend_relationships_query(ctx.user)
             |> Repo.all()
             |> Enum.map(& &1.contact_id)
             |> Enum.sort() ==
               ctx.all_friends |> Enum.map(& &1.id) |> Enum.sort()
    end

    test "should return an error for a non-self user", ctx do
      assert Contacts.friend_relationships_query(ctx.user, ctx.contact) ==
               {:error, :permission_denied}
    end
  end

  describe "get_location_shares/1" do
    test "should not return disabled location shares", ctx do
      Contacts.befriend(ctx.user, ctx.contact, :disabled)

      assert Contacts.get_location_shares(ctx.user) == []
    end
  end

  describe "get_location_sharers/1" do
    test "should not return disabled location shares", ctx do
      Contacts.befriend(ctx.user, ctx.contact, :disabled)

      assert Contacts.get_location_sharers(ctx.contact) == []
    end
  end

  describe "blocks_query/1" do
    test "should return all blocks", ctx do
      Contacts.block(ctx.user, ctx.contact)

      assert [block] = ctx.user |> Contacts.blocks_query() |> Repo.all()
      assert block.user_id == ctx.user.id
      assert block.contact_id == ctx.contact.id
    end
  end

  describe "object_visible_query/3" do
    setup ctx do
      {:ok, bot: Factory.insert(:bot, user: ctx.user)}
    end

    test "should exclude items belonging to blocked users", ctx do
      Contacts.block(ctx.user, ctx.contact)

      query =
        ctx.bot.id
        |> Wocky.POI.get_query()
        |> Contacts.object_visible_query(ctx.contact)

      refute Repo.one(query)

      Contacts.unblock(ctx.user, ctx.contact)

      assert Repo.one(query)
    end
  end
end
