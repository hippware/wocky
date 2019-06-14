defmodule Wocky.Account.AccountTest do
  use Wocky.DataCase, async: true

  alias Ecto.Adapters.SQL
  alias Faker.Internet
  alias Faker.Lorem
  alias Faker.Name
  alias Wocky.Account
  alias Wocky.Account.InviteCode
  alias Wocky.Account.User
  alias Wocky.Block
  alias Wocky.Bots.Invitation
  alias Wocky.Relations
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.Roster
  alias Wocky.TROS
  alias Wocky.TROS.Metadata

  setup do
    user = Factory.insert(:user, device: "testing")

    {:ok,
     user: user,
     id: user.id,
     external_id: user.external_id,
     phone_number: user.phone_number}
  end

  describe "get_user/2" do
    test "should return the requested user", ctx do
      assert %User{} = Account.get_user(ctx.id)
      refute Account.get_user(ID.new())
    end

    test "should not return a blocked user", ctx do
      user2 = Factory.insert(:user)
      Block.block(ctx.user, user2)

      refute Account.get_user(user2.id, ctx.user)
    end
  end

  describe "get_by_phone_number/2" do
    setup do
      users = Factory.insert_list(5, :user)
      {:ok, users: users, phone_numbers: Enum.map(users, & &1.phone_number)}
    end

    test "should return all requested users", ctx do
      assert ctx.phone_numbers
             |> Account.get_by_phone_number(ctx.user)
             |> Enum.sort() == Enum.sort(ctx.users)
    end

    test "should return a single user when requested", ctx do
      assert Account.get_by_phone_number([hd(ctx.users).phone_number], ctx.user) ==
               [hd(ctx.users)]
    end

    test "should not return any elements for unknown phone numbers", ctx do
      assert Account.get_by_phone_number(unused_numbers(ctx.users), ctx.user) ==
               []
    end

    test "should return only used numbers for a mix", ctx do
      assert ctx.phone_numbers
             |> Enum.concat(unused_numbers(ctx.users))
             |> Account.get_by_phone_number(ctx.user)
             |> Enum.sort() == Enum.sort(ctx.users)
    end

    test "should not return blocked users", ctx do
      ctx.users |> tl() |> Enum.each(&Block.block(&1, ctx.user))

      assert ctx.phone_numbers
             |> Account.get_by_phone_number(ctx.user) == [hd(ctx.users)]
    end

    test "should only return one item even for multiple phone numbers", ctx do
      assert ctx.phone_numbers
             |> hd()
             |> List.duplicate(5)
             |> Account.get_by_phone_number(ctx.user) == [hd(ctx.users)]
    end

    defp unused_numbers(phone_numbers) do
      1..5
      |> Enum.map(fn _ -> Factory.phone_number() end)
      |> Enum.uniq()
      |> Kernel.--(phone_numbers)
    end
  end

  describe "first_name/1, last_name/1" do
    test "should split on the last space in a name" do
      u = %User{name: "abc def ghi"}
      assert Account.first_name(u) == "abc def"
      assert Account.last_name(u) == "ghi"
    end

    test "extra spaces should be trimmed" do
      u = %User{name: "   abc    def    ghi    "}
      assert Account.first_name(u) == "abc    def"
      assert Account.last_name(u) == "ghi"
    end

    test "single names should not cause problems" do
      u = %User{name: "えええええええ"}
      assert Account.first_name(u) == ""
      assert Account.last_name(u) == "えええええええ"
    end

    test "empty names should not cause problems" do
      u = %User{name: ""}
      assert Account.first_name(u) == ""
      assert Account.last_name(u) == ""
    end

    test "null names should not cause problems" do
      u = %User{name: nil}
      assert Account.first_name(u) == ""
      assert Account.last_name(u) == ""
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

    test "should fail if the handle has symbols", ctx do
      changeset = set_handle(ctx, "a-bcdef")
      assert errors_on(changeset).handle
    end

    test "should fail if the handle has non-latin characters", ctx do
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
      refute set_name(
               ctx,
               "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
             ).valid?
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
      changeset = User.changeset(ctx.user, %{image_url: "not a valid URL"})
      assert errors_on(changeset).image_url
    end

    test "should fail with a non-existing avatar URL", ctx do
      changeset =
        User.changeset(ctx.user, %{image_url: TROS.make_url(ID.new())})

      assert errors_on(changeset).image_url
    end

    test "should fail with a non-local avatar URL", ctx do
      changeset =
        User.changeset(ctx.user, %{image_url: TROS.make_url(ID.new())})

      assert errors_on(changeset).image_url
    end

    test "should fail with a file owned by another user", ctx do
      changeset =
        :user
        |> Factory.insert()
        |> User.changeset(%{image_url: ctx.url})

      assert errors_on(changeset).image_url
    end

    test "should be valid with valid data", ctx do
      assert User.changeset(ctx.user, %{image_url: ctx.url}).valid?
    end
  end

  describe "update/2" do
    test "should fail when the user does not exist" do
      fields = %{
        device: ID.new(),
        handle: Factory.handle(),
        name: Name.name(),
        email: Internet.email(),
        tagline: Lorem.sentence()
      }

      assert {:error, _} = Account.update(ID.new(), fields)
    end

    test "should update the user's attributes", ctx do
      fields = %{
        device: ID.new(),
        handle: Factory.handle(),
        name: Name.name(),
        email: Internet.email(),
        tagline: Lorem.sentence()
      }

      assert {:ok, _} = Account.update(ctx.id, fields)

      new_user = Repo.get(User, ctx.id)
      assert new_user.handle == fields.handle
      assert new_user.name == fields.name
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
      assert {:ok, _} = Account.update(ctx.id, %{image_url: ctx.avatar_url})

      new_user = Repo.get(User, ctx.id)
      assert new_user.image_url == ctx.avatar_url
    end

    test "and the user already has that avatar", ctx do
      ctx.user
      |> cast(%{image_url: ctx.avatar_url}, [:image_url])
      |> Repo.update!()

      assert {:ok, _} = Account.update(ctx.id, %{image_url: ctx.avatar_url})

      new_user = Repo.get(User, ctx.id)
      assert new_user.image_url == ctx.avatar_url

      assert Repo.get(Metadata, ctx.avatar_id)
    end

    test "and the user has a valid avatar", ctx do
      old_avatar_id = ID.new()
      old_avatar_url = TROS.make_url(old_avatar_id)

      %Metadata{}
      |> Metadata.changeset(%{
        id: old_avatar_id,
        user_id: ctx.user.id,
        access: "public"
      })
      |> Repo.insert!()

      ctx.user
      |> cast(%{image_url: old_avatar_url}, [:image_url])
      |> Repo.update!()

      assert {:ok, _} = Account.update(ctx.id, %{image_url: ctx.avatar_url})

      new_user = Repo.get(User, ctx.id)
      assert new_user.image_url == ctx.avatar_url

      refute Repo.get(Metadata, old_avatar_id)
    end
  end

  describe "avatar deletion when no avatar is set" do
    setup do
      user = Factory.insert(:user, image_url: nil)
      avatar = Factory.insert(:tros_metadata, user: user)

      {:ok, user: user, avatar: avatar, avatar_url: TROS.make_url(avatar.id)}
    end

    test "should not delete the avatar when a new one is set", ctx do
      assert {:ok, _} =
               Account.update(ctx.user.id, %{image_url: ctx.avatar_url})

      assert Repo.get(Metadata, ctx.avatar.id)
    end

    test "should not delete the avatar when a new one is not set", ctx do
      assert {:ok, _} = Account.update(ctx.user.id, %{name: Name.name()})

      assert Repo.get(Metadata, ctx.avatar.id)
    end

    test "should not delete the avatar when the same one is set", ctx do
      assert {:ok, _} =
               Account.update(ctx.user.id, %{image_url: ctx.user.image_url})

      assert Repo.get(Metadata, ctx.avatar.id)
    end
  end

  describe "avatar deletion when an avatar is set" do
    setup do
      user = Factory.insert(:user, image_url: nil)
      avatar = Factory.insert(:tros_metadata, user: user)
      avatar_url = TROS.make_url(avatar.id)

      Account.update(user.id, %{image_url: avatar_url})

      new_avatar = Factory.insert(:tros_metadata, user: user)

      {:ok,
       user: user,
       avatar: avatar,
       new_avatar: new_avatar,
       avatar_url: avatar_url,
       new_avatar_url: TROS.make_url(new_avatar.id)}
    end

    test "should delete the avatar when a new one is set", ctx do
      assert {:ok, _} =
               Account.update(ctx.user.id, %{image_url: ctx.new_avatar_url})

      refute Repo.get(Metadata, ctx.avatar.id)
    end

    test "should not delete the avatar when one is not set", ctx do
      assert {:ok, _} = Account.update(ctx.user.id, %{name: Name.name()})

      assert Repo.get(Metadata, ctx.avatar.id)
    end

    test "should not delete the avatar when the same one is set", ctx do
      assert {:ok, _} =
               Account.update(ctx.user.id, %{image_url: ctx.user.image_url})

      assert Repo.get(Metadata, ctx.avatar.id)
    end
  end

  describe "delete/1" do
    test "should remove the user from the database", ctx do
      assert Account.delete(ctx.id) == :ok
      refute Repo.get(User, ctx.id)
    end

    test "should delete the user's TROS files", ctx do
      files = Factory.insert_list(5, :tros_metadata, user: ctx.user)

      assert Account.delete(ctx.id) == :ok

      actions = TROS.Store.Test.get_actions() |> Enum.sort()
      expected = files |> Enum.map(&{:delete, &1.id}) |> Enum.sort()

      assert actions == expected
    end

    test "should succeed if the user does not exist" do
      assert Account.delete(ID.new()) == :ok
    end
  end

  defp setup_bot_relationships(ctx) do
    other_user = Factory.insert(:user)
    Roster.befriend(ctx.user, other_user)

    owned_bot = Factory.insert(:bot, user: ctx.user)
    pending_bot = Factory.insert(:bot, user: ctx.user, pending: true)
    invited_bot = Factory.insert(:bot, user: other_user)
    subscribed_bot = Factory.insert(:bot, user: other_user)
    unaffiliated_bot = Factory.insert(:bot, user: other_user)

    Invitation.put(ctx.user, invited_bot, other_user)
    Relations.subscribe(subscribed_bot, ctx.user)

    {:ok,
     other_user: other_user,
     owned_bot: owned_bot,
     pending_bot: pending_bot,
     invited_bot: invited_bot,
     subscribed_bot: subscribed_bot,
     unaffiliated_bot: unaffiliated_bot}
  end

  describe "searchable checks" do
    setup :setup_bot_relationships

    setup ctx do
      friend = Factory.insert(:user)
      RosterHelper.make_friends(friend, ctx.user)

      friends_private_bot = Factory.insert(:bot, user: friend)
      friends_invited_private_bot = Factory.insert(:bot, user: friend)

      Invitation.put(ctx.user, friends_invited_private_bot, friend)

      {:ok,
       friends_private_bot: friends_private_bot,
       friends_invited_private_bot: friends_invited_private_bot}
    end

    test "searchable stored procedure", ctx do
      assert is_searchable_sp(ctx.user, ctx.owned_bot)
      assert is_searchable_sp(ctx.user, ctx.subscribed_bot)
      refute is_searchable_sp(ctx.user, ctx.friends_invited_private_bot)
      refute is_searchable_sp(ctx.user, ctx.invited_bot)
      refute is_searchable_sp(ctx.user, ctx.unaffiliated_bot)
      refute is_searchable_sp(ctx.user, ctx.friends_private_bot)
    end
  end

  describe "make_invite_code/1" do
    setup %{user: user} do
      code = Account.make_invite_code(user)
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
      code = Account.make_invite_code(user)
      user = Factory.insert(:user)
      {:ok, code: code, redeemer: user}
    end

    test "it should friend the current user and the owner of the code", ctx do
      assert Account.redeem_invite_code(ctx.redeemer, ctx.code)
      assert Roster.friend?(ctx.user, ctx.redeemer)
    end

    test "it should return true if the redeemer created the code", ctx do
      assert Account.redeem_invite_code(ctx.user, ctx.code)
    end

    test "it should return false if the code is bad", ctx do
      bad_code = InviteCode.generate()
      refute Account.redeem_invite_code(ctx.redeemer, bad_code)
    end

    test "it should return false if the code is expired", ctx do
      invitation = Repo.get_by(InviteCode, code: ctx.code)
      assert invitation

      ts = Timex.shift(invitation.created_at, days: -31)

      invitation
      |> Ecto.Changeset.change(created_at: ts)
      |> Repo.update!()

      refute Account.redeem_invite_code(ctx.redeemer, ctx.code)
    end

    test "it should return false if the redeemer is blocked", ctx do
      Block.block(ctx.user, ctx.redeemer)
      refute Account.redeem_invite_code(ctx.redeemer, ctx.code)
    end
  end

  describe "sms_allowed_inc?" do
    test "should allow SMS for a new user", ctx do
      assert Account.sms_allowed_inc?(ctx.user)
    end

    test "should increment the amount sent", ctx do
      assert Account.get_user(ctx.user.id).smss_sent == 0
      Account.sms_allowed_inc?(ctx.user)
      assert Account.get_user(ctx.user.id).smss_sent == 1
    end

    test "should reject a non-existant user" do
      u = Factory.build(:user)
      refute Account.sms_allowed_inc?(u)
    end

    test "should reject when the user has sent the max SMSs" do
      max = Confex.get_env(:wocky, :max_sms_per_user)
      u = Factory.insert(:user, smss_sent: max)
      refute Account.sms_allowed_inc?(u)
      assert Account.get_user(u.id).smss_sent == max
    end
  end

  describe "flag_bot_created/1" do
    test "should set the user as having a created bot", ctx do
      refute Repo.get(User, ctx.user.id).bot_created
      assert :ok == Account.flag_bot_created(ctx.user)
      assert Repo.get(User, ctx.user.id).bot_created
    end

    test "should have no effect if the flag is already set" do
      user = Factory.insert(:user, bot_created: true)
      assert :ok == Account.flag_bot_created(user)
      assert Repo.get(User, user.id).bot_created
    end
  end

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

  defp set_name(ctx, name),
    do: User.changeset(ctx.user, %{name: name})
end
