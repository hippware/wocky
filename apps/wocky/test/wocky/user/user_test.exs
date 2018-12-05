defmodule Wocky.User.UserTest do
  use Wocky.DataCase

  use Wocky.JID

  alias Ecto.Adapters.SQL
  alias Faker.Internet
  alias Faker.Lorem
  alias Faker.Name
  alias Wocky.Block
  alias Wocky.Bot
  alias Wocky.Bot.Invitation
  alias Wocky.Repo
  alias Wocky.Repo.{Factory, ID, Timestamp}
  alias Wocky.Roster
  alias Wocky.TROS
  alias Wocky.TROS.Metadata
  alias Wocky.User
  alias Wocky.User.BotEvent
  alias Wocky.User.InviteCode
  alias Wocky.User.Location

  setup do
    user = Factory.insert(:user, device: "testing")

    {:ok,
     user: user,
     id: user.id,
     external_id: user.external_id,
     phone_number: user.phone_number}
  end

  describe "valid_update_fields/0" do
    assert length(User.valid_update_fields()) == 10
  end

  describe "to_jid/1" do
    test "should return the user's JID", ctx do
      jid1 = User.to_jid(ctx.user)
      jid2 = JID.make(ctx.user.id, Wocky.host(), ctx.user.device)

      assert JID.equal?(jid1, jid2)
    end
  end

  describe "get_by_jid/1" do
    test "when the user exists", ctx do
      result = ctx.user |> User.to_jid() |> User.get_by_jid()

      assert result.id == ctx.user.id
      assert result.device == ctx.user.device
    end

    test "when the user does not exist" do
      refute ID.new() |> JID.make() |> User.get_by_jid()
    end

    test "when the jid has no user ID" do
      refute "" |> JID.make() |> User.get_by_jid()
    end
  end

  describe "get_user/2" do
    test "should return the requested user", ctx do
      assert %User{} = User.get_user(ctx.id)
      refute User.get_user(ID.new())
    end

    test "should not return a blocked user", ctx do
      user2 = Factory.insert(:user)
      Block.block(ctx.user, user2)

      refute User.get_user(user2.id, ctx.user)
    end
  end

  describe "changeset/1 validations" do
    test "should pass with valid attributes", ctx do
      attrs = %{handle: "new_handle", email: "foo@bar.com"}

      assert User.changeset(ctx.user, attrs).valid?
    end

    test "should fail if the email is malformed", ctx do
      changeset = User.changeset(ctx.user, %{email: "foo"})
      assert errors_on(changeset).email
    end

    test "should fail with a reserved handle", ctx do
      changeset = set_handle(ctx, "Root")
      assert errors_on(changeset).handle
    end

    test "should fail if it contains a reserved handle", ctx do
      changeset = set_handle(ctx, "wWwgf")
      assert errors_on(changeset).handle
    end

    test "should fail if the handle has invalid characters", ctx do
      changeset = set_handle(ctx, "a-bcdef")
      assert errors_on(changeset).handle

      changeset = set_handle(ctx, "abąab")
      assert errors_on(changeset).handle
    end

    test "should fail if the handle is too short", ctx do
      changeset = set_handle(ctx, "ab")
      assert errors_on(changeset).handle
    end

    test "should fail if the handle is too long", ctx do
      changeset = set_handle(ctx, "abcdefghijklmnopq")
      assert errors_on(changeset).handle
    end

    test "should succeed for valid handles within the correct length", ctx do
      assert set_handle(ctx, "abc").valid?
      assert set_handle(ctx, "abcdefghijklmnop").valid?
    end

    test "should fail if the name starts or ends with a space or hyphen", ctx do
      refute set_name(ctx, "-abdc").valid?
      refute set_name(ctx, " abdc").valid?
      refute set_name(ctx, "abdc ").valid?
      refute set_name(ctx, "abdc-").valid?
    end

    test "should fail if the name starts with a digit", ctx do
      refute set_name(ctx, "5aaaa").valid?
    end

    test "should succeed with characters, digits and spaces in the middle",
         ctx do
      assert set_name(ctx, "Ƶ𝓾 え-09").valid?
    end

    test "should accept a name up to the 32 character limit", ctx do
      assert set_name(ctx, "ええええええええええええええええええええええええええええええええ").valid?
    end

    test "should fail a name with more than the limit", ctx do
      refute set_name(ctx, "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa").valid?
    end

    test "should accept an empty name", ctx do
      assert set_name(ctx, "").valid?
    end

    test "when there is a pre-existing matching handle regardless of case",
         ctx do
      assert {:error, changeset} =
               :user
               |> Factory.insert()
               |> User.changeset(%{handle: String.upcase(ctx.user.handle)})
               |> Repo.update()

      assert errors_on(changeset).handle
    end
  end

  describe "avatar validations" do
    setup ctx do
      %Metadata{id: file_id} =
        Factory.insert(
          :tros_metadata,
          user: ctx.user,
          access: "public"
        )

      url = TROS.make_url(file_id)
      {:ok, file_id: file_id, url: url}
    end

    test "should fail with an invalid avatar URL", ctx do
      changeset = User.changeset(ctx.user, %{avatar: "not a valid URL"})
      assert errors_on(changeset).avatar
    end

    test "should fail with a non-existing avatar URL", ctx do
      changeset = User.changeset(ctx.user, %{avatar: TROS.make_url(ID.new())})
      assert errors_on(changeset).avatar
    end

    test "should fail with a non-local avatar URL", ctx do
      changeset = User.changeset(ctx.user, %{avatar: TROS.make_url(ID.new())})
      assert errors_on(changeset).avatar
    end

    test "should fail with a file owned by another user", ctx do
      changeset =
        :user
        |> Factory.insert()
        |> User.changeset(%{avatar: ctx.url})

      assert errors_on(changeset).avatar
    end

    test "should be valid with valid data", ctx do
      assert User.changeset(ctx.user, %{avatar: ctx.url}).valid?
    end
  end

  describe "update/2" do
    test "should fail when the user does not exist" do
      fields = %{
        device: ID.new(),
        handle: Factory.new_handle(),
        first_name: Name.first_name(),
        last_name: Name.last_name(),
        email: Internet.email(),
        tagline: Lorem.sentence()
      }

      assert {:error, _} = User.update(ID.new(), fields)
    end

    test "should update the user's attributes", ctx do
      fields = %{
        device: ID.new(),
        handle: Factory.new_handle(),
        first_name: Name.first_name(),
        last_name: Name.last_name(),
        email: Internet.email(),
        tagline: Lorem.sentence()
      }

      assert {:ok, _} = User.update(ctx.id, fields)

      new_user = Repo.get(User, ctx.id)
      assert new_user.handle == fields.handle
      assert new_user.first_name == fields.first_name
      assert new_user.last_name == fields.last_name
      assert new_user.email == fields.email
      assert new_user.tagline == fields.tagline
      refute new_user.device
    end
  end

  describe "update/2 when a valid avatar is passed" do
    setup ctx do
      %Metadata{id: id} =
        Factory.insert(
          :tros_metadata,
          user: ctx.user,
          access: "public"
        )

      avatar_url = TROS.make_url(id)

      {:ok, avatar_id: id, avatar_url: avatar_url}
    end

    test "and the user does not have an existing avatar", ctx do
      assert {:ok, _} = User.update(ctx.id, %{avatar: ctx.avatar_url})

      new_user = Repo.get(User, ctx.id)
      assert new_user.avatar == ctx.avatar_url
    end

    test "and the user already has that avatar", ctx do
      ctx.user
      |> cast(%{avatar: ctx.avatar_url}, [:avatar])
      |> Repo.update!()

      assert {:ok, _} = User.update(ctx.id, %{avatar: ctx.avatar_url})

      new_user = Repo.get(User, ctx.id)
      assert new_user.avatar == ctx.avatar_url

      assert Metadata.get(ctx.avatar_id)
    end

    test "and the user has a valid avatar", ctx do
      old_avatar_id = ID.new()
      old_avatar_url = TROS.make_url(old_avatar_id)
      Metadata.put(old_avatar_id, ctx.user.id, "public")

      ctx.user
      |> cast(%{avatar: old_avatar_url}, [:avatar])
      |> Repo.update!()

      assert {:ok, _} = User.update(ctx.id, %{avatar: ctx.avatar_url})

      new_user = Repo.get(User, ctx.id)
      assert new_user.avatar == ctx.avatar_url

      refute Metadata.get(old_avatar_id)
    end
  end

  describe "set_location/2" do
    setup ctx do
      user2 = Factory.insert(:user)
      bot = Factory.insert(:bot, user: user2)

      Bot.subscribe(bot, ctx.user)

      {:ok, bot: bot, lat: Bot.lat(bot), lon: Bot.lon(bot)}
    end

    test "should save the location to the database", ctx do
      location = %Location{
        lat: ctx.lat,
        lon: ctx.lon,
        accuracy: 10,
        device: "testing",
        captured_at: DateTime.utc_now()
      }

      assert {:ok, %Location{id: id}} = User.set_location(ctx.user, location)
      assert Repo.get(Location, id)
    end

    test "should initiate geofence processing", ctx do
      assert User.set_location(ctx.user, "testing", ctx.lat, ctx.lon, 10) == :ok
      assert BotEvent.get_last_event_type(ctx.id, ctx.bot.id) == :transition_in
    end

    test "should not initiate geofence processing if the user is hidden", ctx do
      {:ok, user} = User.hide(ctx.user, true)

      assert User.set_location(user, "testing", ctx.lat, ctx.lon, 10) == :ok
      refute BotEvent.get_last_event(user.id, ctx.bot.id)
    end
  end

  describe "set_location_for_bot/3" do
    setup ctx do
      user2 = Factory.insert(:user)
      bot = Factory.insert(:bot, user: user2)

      Bot.subscribe(bot, ctx.user)

      location = %Location{
        lat: Bot.lat(bot),
        lon: Bot.lon(bot),
        accuracy: 10,
        device: "testing",
        captured_at: DateTime.utc_now()
      }

      {:ok, bot: bot, location: location}
    end

    test "should save the location to the database", ctx do
      assert {:ok, %Location{id: id}} =
               User.set_location_for_bot(ctx.user, ctx.location, ctx.bot)

      assert Repo.get(Location, id)
    end

    test "should initiate geofence processing for that bot", ctx do
      assert {:ok, _} =
               User.set_location_for_bot(ctx.user, ctx.location, ctx.bot)

      assert Bot.subscription(ctx.bot, ctx.user) == :visiting
    end

    test "should not initiate geofence processing if the user is hidden", ctx do
      {:ok, user} = User.hide(ctx.user, true)

      assert {:ok, _} = User.set_location_for_bot(user, ctx.location, ctx.bot)
      assert Bot.subscription(ctx.bot, ctx.user) == :subscribed
    end
  end

  describe "delete/1" do
    test "should remove the user from the database", ctx do

      assert User.delete(ctx.id) == :ok
      refute Repo.get(User, ctx.id)
    end

    test "should delete the user's TROS files", ctx do
      files = Factory.insert_list(5, :tros_metadata, user: ctx.user)

      assert User.delete(ctx.id) == :ok

      actions = TROS.Store.Test.get_actions() |> Enum.sort()
      expected = files |> Enum.map(&{:delete, &1.id}) |> Enum.sort()

      assert actions == expected
    end

    test "should succeed if the user does not exist" do
      assert User.delete(ID.new()) == :ok
    end
  end

  describe "get_locations_query/2" do
    setup ctx do
      Factory.insert_list(5, :location, user_id: ctx.id, device: "test")

      :ok
    end

    test "should return a query for retrieving user locations", ctx do
      query = User.get_locations_query(ctx.user, "test")

      assert query |> Repo.all() |> length() == 5
    end
  end

  defp setup_roles(ctx) do
    role = Lorem.word()
    role2 = "role2"
    result = User.add_role(ctx.id, role)
    result2 = User.add_role(ctx.id, role2)

    {:ok, role: role, role2: role2, result: result, result2: result2}
  end

  describe "add_role/2" do
    setup :setup_roles

    test "should add the role to the user", ctx do
      assert ctx.result == :ok
      assert ctx.result2 == :ok

      roles =
        User
        |> Repo.get(ctx.id)
        |> Map.get(:roles)
        |> Enum.sort()

      assert roles == Enum.sort([ctx.role, ctx.role2])
    end

    test "should not add re-add the same group", ctx do
      User.add_role(ctx.id, ctx.role)

      roles =
        User
        |> Repo.get(ctx.id)
        |> Map.get(:roles)
        |> Enum.sort()

      assert roles == Enum.sort([ctx.role, ctx.role2])
    end

    test "should nd not fail when acting on an inavlid user" do
      assert User.add_role(ID.new(), Lorem.word()) == :ok
    end
  end

  describe "remove_role/2" do
    setup :setup_roles

    setup ctx do
      result = User.remove_role(ctx.id, ctx.role)

      {:ok, result: result}
    end

    test "should remove the role form the user", ctx do
      assert ctx.result == :ok

      user = Repo.get(User, ctx.id)

      assert user.roles == [ctx.role2]
    end

    test "should not have any effect when applied again", ctx do
      assert User.remove_role(ctx.id, ctx.role) == :ok

      user = Repo.get(User, ctx.id)

      assert user.roles == [ctx.role2]
    end

    test "should nd not fail when acting on an inavlid user" do
      assert User.remove_role(ID.new(), Lorem.word()) == :ok
    end
  end

  defp setup_bot_relationships(ctx) do
    other_user = Factory.insert(:user)

    owned_bot = Factory.insert(:bot, user: ctx.user)
    pending_bot = Factory.insert(:bot, user: ctx.user, pending: true)
    invited_bot = Factory.insert(:bot, user: other_user)
    subscribed_bot = Factory.insert(:bot, user: other_user)
    unaffiliated_bot = Factory.insert(:bot, user: other_user)

    Invitation.put(ctx.user, invited_bot, other_user)
    Bot.subscribe(subscribed_bot, ctx.user)

    {:ok,
     other_user: other_user,
     owned_bot: owned_bot,
     pending_bot: pending_bot,
     invited_bot: invited_bot,
     subscribed_bot: subscribed_bot,
     unaffiliated_bot: unaffiliated_bot}
  end

  describe "bot relationships" do
    setup :setup_bot_relationships

    test "owns?/2", ctx do
      assert User.owns?(ctx.user, ctx.owned_bot)
      refute User.owns?(ctx.user, ctx.unaffiliated_bot)
    end

    test "can_access?/2", ctx do
      assert User.can_access?(ctx.user, ctx.owned_bot)
      assert User.can_access?(ctx.user, ctx.invited_bot)
      refute User.can_access?(ctx.user, ctx.unaffiliated_bot)
    end

    test "get_subscriptions/1", ctx do
      subscriptions = User.get_subscriptions(ctx.user)

      assert length(subscriptions) == 1
      assert Enum.any?(subscriptions, &same_bot(&1, ctx.subscribed_bot))
      refute Enum.any?(subscriptions, &same_bot(&1, ctx.owned_bot))
      refute Enum.any?(subscriptions, &same_bot(&1, ctx.pending_bot))
    end

    test "bot_count/1", ctx do
      assert User.bot_count(ctx.user) == 1
      assert User.bot_count(ctx.other_user) == 3
    end

    test "get_owned_bots/1", ctx do
      bots = User.get_owned_bots(ctx.user)

      assert length(bots) == 1
      assert Enum.any?(bots, &same_bot(&1, ctx.owned_bot))
      refute Enum.any?(bots, &same_bot(&1, ctx.pending_bot))
    end
  end

  describe "searchable checks" do
    setup :setup_bot_relationships

    setup ctx do
      friend = Factory.insert(:user)
      RosterHelper.make_friends(friend, ctx.user)

      followee = Factory.insert(:user)
      RosterHelper.follow(ctx.user, followee)

      friends_private_bot = Factory.insert(:bot, user: friend)
      friends_invited_private_bot = Factory.insert(:bot, user: friend)

      following_private_bot = Factory.insert(:bot, user: followee)
      following_invited_private_bot = Factory.insert(:bot, user: followee)

      Invitation.put(ctx.user, friends_invited_private_bot, friend)
      Invitation.put(ctx.user, following_invited_private_bot, followee)

      {:ok,
       friends_private_bot: friends_private_bot,
       friends_invited_private_bot: friends_invited_private_bot,
       following_private_bot: following_private_bot,
       following_invited_private_bot: following_invited_private_bot}
    end

    test "searchable?/2", ctx do
      assert User.searchable?(ctx.user, ctx.subscribed_bot)
      refute User.searchable?(ctx.user, ctx.friends_invited_private_bot)
      refute User.searchable?(ctx.user, ctx.invited_bot)
      refute User.searchable?(ctx.user, ctx.unaffiliated_bot)
      refute User.searchable?(ctx.user, ctx.friends_private_bot)
      refute User.searchable?(ctx.user, ctx.following_private_bot)
      refute User.searchable?(ctx.user, ctx.following_invited_private_bot)
    end

    test "searchable stored procedure", ctx do
      assert is_searchable_sp(ctx.user, ctx.owned_bot)
      assert is_searchable_sp(ctx.user, ctx.subscribed_bot)
      refute is_searchable_sp(ctx.user, ctx.friends_invited_private_bot)
      refute is_searchable_sp(ctx.user, ctx.invited_bot)
      refute is_searchable_sp(ctx.user, ctx.unaffiliated_bot)
      refute is_searchable_sp(ctx.user, ctx.friends_private_bot)
      refute is_searchable_sp(ctx.user, ctx.following_private_bot)
      refute is_searchable_sp(ctx.user, ctx.following_invited_private_bot)
    end
  end

  describe "avatar deletion when no avatar is set" do
    setup do
      user = Factory.insert(:user, avatar: nil)
      avatar = Factory.insert(:tros_metadata, user: user)

      {:ok, user: user, avatar: avatar, avatar_url: TROS.make_url(avatar.id)}
    end

    test "should not delete the avatar when a new one is set", ctx do
      assert {:ok, _} = User.update(ctx.user.id, %{avatar: ctx.avatar_url})
      assert Metadata.get(ctx.avatar.id)
    end

    test "should not delete the avatar when a new one is not set", ctx do
      assert {:ok, _} =
               User.update(ctx.user.id, %{first_name: Name.first_name()})

      assert Metadata.get(ctx.avatar.id)
    end

    test "should not delete the avatar when the same one is set", ctx do
      assert {:ok, _} = User.update(ctx.user.id, %{avatar: ctx.user.avatar})
      assert Metadata.get(ctx.avatar.id)
    end
  end

  describe "avatar deletion when an avatar is set" do
    setup do
      user = Factory.insert(:user, avatar: nil)
      avatar = Factory.insert(:tros_metadata, user: user)
      avatar_url = TROS.make_url(avatar.id)

      User.update(user.id, %{avatar: avatar_url})

      new_avatar = Factory.insert(:tros_metadata, user: user)

      {:ok,
       user: user,
       avatar: avatar,
       new_avatar: new_avatar,
       avatar_url: avatar_url,
       new_avatar_url: TROS.make_url(new_avatar.id)}
    end

    test "should delete the avatar when a new one is set", ctx do
      assert {:ok, _} = User.update(ctx.user.id, %{avatar: ctx.new_avatar_url})
      refute Metadata.get(ctx.avatar.id)
    end

    test "should not delete the avatar when one is not set", ctx do
      assert {:ok, _} =
               User.update(ctx.user.id, %{first_name: Name.first_name()})

      assert Metadata.get(ctx.avatar.id)
    end

    test "should not delete the avatar when the same one is set", ctx do
      assert {:ok, _} = User.update(ctx.user.id, %{avatar: ctx.user.avatar})
      assert Metadata.get(ctx.avatar.id)
    end
  end

  test "full_name/1", ctx do
    assert ctx.user |> User.full_name() |> is_binary()
  end

  describe "remove_auth_details/1" do
    test "valid user", ctx do
      assert User.remove_auth_details(ctx.user.id) == :ok

      user = Repo.get(User, ctx.user.id)
      refute user.phone_number
      refute user.provider
      refute user.external_id
    end

    test "invalid user" do
      assert User.remove_auth_details(ID.new()) == :ok
    end
  end

  describe "hiding" do
    test "should set the user hidden forever", ctx do
      {:ok, user} = User.hide(ctx.user, true)

      assert user.hidden_until == User.forever_ts()
      assert User.hidden_state(user) == {true, nil}
      assert User.hidden?(user)

      id = ctx.id
      query = User |> where(id: ^id) |> User.filter_hidden()
      assert Repo.all(query) == []
    end

    test "should set the user hidden for a limited time", ctx do
      expire = Timestamp.shift(days: 1)
      {:ok, user} = User.hide(ctx.user, expire)

      assert user.hidden_until == expire
      assert User.hidden_state(user) == {true, expire}
      assert User.hidden?(user)

      id = ctx.id
      query = User |> where(id: ^id) |> User.filter_hidden()
      assert Repo.all(query) == []
    end

    test "should unhide a hidden user", ctx do
      {:ok, user} = User.hide(ctx.user, false)

      refute user.hidden_until
      assert User.hidden_state(user) == {false, nil}
      refute User.hidden?(user)

      id = ctx.id
      query = User |> where(id: ^id) |> User.filter_hidden()
      refute Repo.all(query) == []
    end

    test "should unhide a user whose hiding expired", ctx do
      expire = Timestamp.shift(days: -1)
      {:ok, user} = User.hide(ctx.user, expire)

      assert user.hidden_until == expire
      assert User.hidden_state(user) == {false, expire}
      refute User.hidden?(user)

      id = ctx.id
      query = User |> where(id: ^id) |> User.filter_hidden()
      refute Repo.all(query) == []
    end
  end

  describe "make_invite_code/1" do
    setup %{user: user} do
      code = User.make_invite_code(user)
      {:ok, code: code}
    end

    test "it should generate a string code", ctx do
      assert is_binary(ctx.code)
    end

    test "it should store the code in the database", ctx do
      assert Repo.get_by(InviteCode, user_id: ctx.user.id, code: ctx.code)
    end
  end

  describe "redeem_invite_code/2" do
    setup %{user: user} do
      code = User.make_invite_code(user)
      user = Factory.insert(:user)
      {:ok, code: code, redeemer: user}
    end

    test "it should friend the current user and the owner of the code", ctx do
      assert User.redeem_invite_code(ctx.redeemer, ctx.code)
      assert Roster.friend?(ctx.user.id, ctx.redeemer.id)
    end

    test "it should return true if the redeemer created the code", ctx do
      assert User.redeem_invite_code(ctx.user, ctx.code)
    end

    test "it should return false if the code is bad", ctx do
      bad_code = InviteCode.generate()
      refute User.redeem_invite_code(ctx.redeemer, bad_code)
    end

    test "it should return false if the code is expired", ctx do
      invitation = Repo.get_by(InviteCode, code: ctx.code)
      assert invitation

      ts = Timex.shift(invitation.created_at, days: -31)

      invitation
      |> Ecto.Changeset.change(created_at: ts)
      |> Repo.update!()

      refute User.redeem_invite_code(ctx.redeemer, ctx.code)
    end

    test "it should return false if the redeemer is blocked", ctx do
      Block.block(ctx.user, ctx.redeemer)
      refute User.redeem_invite_code(ctx.redeemer, ctx.code)
    end
  end

  defp same_bot(bot1, bot2), do: bot1.id == bot2.id

  defp is_searchable_sp(user, bot),
    do: run_stored_proc(user, bot, "is_searchable")

  defp run_stored_proc(user, bot, proc) do
    {:ok, u} = Ecto.UUID.dump(user.id)
    {:ok, b} = Ecto.UUID.dump(bot.id)

    result =
      SQL.query!(
        Repo,
        "SELECT * FROM bots AS bot WHERE id = $2 AND #{proc}($1, bot)",
        [u, b]
      )

    length(result.rows) == 1
  end

  defp set_handle(ctx, handle),
    do: User.changeset(ctx.user, %{handle: handle})

  defp set_name(ctx, name) do
    field = Enum.random([:first_name, :last_name])
    User.changeset(ctx.user, %{field => name})
  end
end
