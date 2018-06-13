# credo:disable-for-this-file Credo.Check.Refactor.PipeChainStart
defmodule Wocky.UserSpec do
  use ESpec, async: true
  use ModelHelpers
  use Wocky.JID

  alias Ecto.Adapters.SQL
  alias Faker.Internet
  alias Faker.Lorem
  alias Faker.Name
  alias Wocky.Account
  alias Wocky.Account.Token
  alias Wocky.Block
  alias Wocky.Bot
  alias Wocky.Bot.Share
  alias Wocky.Email
  alias Wocky.Index.TestIndexer
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.TROS
  alias Wocky.TROS.Metadata
  alias Wocky.User

  before do
    user = Factory.insert(:user, resource: "testing")

    {:ok,
     user: user,
     id: user.id,
     external_id: user.external_id,
     phone_number: user.phone_number}
  end

  describe "valid_update_fields/0" do
    subject do: User.valid_update_fields()

    it do: should(have_count 9)
  end

  describe "to_jid/1" do
    it "should return the user's JID" do
      jid1 = User.to_jid(shared.user)
      jid2 = JID.make(shared.user.id, Wocky.host(), shared.user.resource)
      jid1 |> JID.equal?(jid2) |> should(be_true())
    end
  end

  describe "get_by_jid/1" do
    context "when the user exists" do
      subject do: shared.user |> User.to_jid() |> User.get_by_jid()

      it do: subject().id |> should(eq shared.user.id)
      it do: subject().resource |> should(eq shared.user.resource)
    end

    context "when the user does not exist" do
      subject do: ID.new() |> JID.make() |> User.get_by_jid()

      it do: should(be_nil())
    end

    context "when the jid has no user ID" do
      subject do: "" |> JID.make() |> User.get_by_jid()

      it do: should(be_nil())
    end
  end

  describe "changeset/1 validations" do
    it "should pass with valid attributes" do
      shared.user
      |> User.changeset(%{handle: "new_handle", email: "foo@bar.com"})
      |> should(be_valid())
    end

    it "should fail if the email is malformed" do
      shared.user
      |> User.changeset(%{email: "foo"})
      |> should(have_errors([:email]))
    end

    it "should fail with a reserved handle" do
      set_handle(shared, "Root")
      |> should(have_errors([:handle]))
    end

    it "should fail if it contains a reserved handle" do
      set_handle(shared, "wWwgf")
      |> should(have_errors([:handle]))
    end

    it "should fail if the handle has invalid characters" do
      set_handle(shared, "a-bcdef")
      |> should(have_errors([:handle]))

      set_handle(shared, "abąab")
      |> should(have_errors([:handle]))
    end

    it "should fail if the handle is too short" do
      set_handle(shared, "ab")
      |> should(have_errors([:handle]))
    end

    it "should fail if the handle is too long" do
      set_handle(shared, "abcdefghijklmnopq")
      |> should(have_errors([:handle]))
    end

    it "should succeed for valid handles within the correct length" do
      set_handle(shared, "abc")
      |> should(be_valid())

      set_handle(shared, "abcdefghijklmnop")
      |> should(be_valid())
    end

    it "should fail if the name starts or ends with a space or hyphen" do
      set_name(shared, "-abdc")
      |> should_not(be_valid())

      set_name(shared, " abdc")
      |> should_not(be_valid())

      set_name(shared, "abdc ")
      |> should_not(be_valid())

      set_name(shared, "abdc-")
      |> should_not(be_valid())
    end

    it "should fail if the name starts with a digit" do
      set_name(shared, "5aaaa")
      |> should_not(be_valid())
    end

    it "should succeed with characters, digits and spaces in the middle" do
      set_name(shared, "Ƶ𝓾 え-09")
      |> should(be_valid())
    end

    it "should accept a name up to the 32 character limit" do
      set_name(shared, "ええええええええええええええええええええええええええええええええ")
      |> should(be_valid())
    end

    it "should fail a name with more than the limit" do
      set_name(shared, "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
      |> should_not(be_valid())
    end

    it "should accept an empty name" do
      set_name(shared, "")
      |> should(be_valid())
    end

    context "when there is a pre-existing matching handle regardless of case" do
      subject do
        :user
        |> Factory.insert()
        |> User.changeset(%{handle: String.upcase(shared.user.handle)})
        |> Repo.update()
      end

      it do: should(be_error_result())
      it do: subject() |> Kernel.elem(1) |> should(have_errors([:handle]))
    end

    context "avatar validations" do
      before do
        %Metadata{id: file_id} =
          Factory.insert(
            :tros_metadata,
            user: shared.user,
            access: "public"
          )

        url = TROS.make_url(file_id)
        {:ok, file_id: file_id, url: url}
      end

      context "with invalid data" do
        it "should fail with an invalid avatar URL" do
          shared.user
          |> User.changeset(%{avatar: "not a valid URL"})
          |> should(have_errors([:avatar]))
        end

        it "should fail with a non-existing avatar URL" do
          shared.user
          |> User.changeset(%{avatar: TROS.make_url(ID.new())})
          |> should(have_errors([:avatar]))
        end

        it "should fail with a non-local avatar URL" do
          shared.user
          |> User.changeset(%{avatar: TROS.make_url(ID.new())})
          |> should(have_errors([:avatar]))
        end

        it "should fail with a file owned by another user" do
          :user
          |> Factory.insert()
          |> User.changeset(%{avatar: shared.url})
          |> should(have_errors([:avatar]))
        end
      end

      context "with valid data" do
        it "should be valid" do
          shared.user
          |> User.changeset(%{avatar: shared.url})
          |> should(be_valid())
        end
      end
    end
  end

  describe "tests using TestIndexer", async: false do
    before do
      TestIndexer.reset()
    end

    describe "update/2", async: false do
      before do
        :meck.new(Email)
        :meck.expect(Email, :send_welcome_email, fn %User{} -> :ok end)
      end

      finally do
        :meck.unload(Email)
      end

      context "when the user does not exist" do
        before do
          fields = %{
            resource: ID.new(),
            handle: Factory.new_handle(),
            first_name: Name.first_name(),
            last_name: Name.last_name(),
            email: Internet.email(),
            tagline: Lorem.sentence()
          }

          result = User.update(ID.new(), fields)
          {:ok, fields: fields, result: result}
        end

        it "should return an error" do
          shared.result |> should(be_error_result())
        end
      end

      context "standard update" do
        before do
          fields = %{
            resource: ID.new(),
            handle: Factory.new_handle(),
            first_name: Name.first_name(),
            last_name: Name.last_name(),
            email: Internet.email(),
            tagline: Lorem.sentence()
          }

          result = User.update(shared.id, fields)
          {:ok, fields: fields, result: result}
        end

        it "should return :ok" do
          shared.result |> should(be_ok_result())
        end

        it "should update the user's attributes" do
          new_user = Repo.get(User, shared.id)
          new_user.handle |> should(eq shared.fields.handle)
          new_user.first_name |> should(eq shared.fields.first_name)
          new_user.last_name |> should(eq shared.fields.last_name)
          new_user.email |> should(eq shared.fields.email)
          new_user.tagline |> should(eq shared.fields.tagline)
        end

        it "should not update the user's resource" do
          new_user = Repo.get(User, shared.id)
          new_user.resource |> should(be_nil())
        end

        context "full text search index" do
          it "should be updated" do
            TestIndexer.get_index_operations() |> should_not(be_empty())
          end
        end

        it "should trigger the sending of a welcome email" do
          :meck.validate(Email) |> should(be_true())
          :meck.called(Email, :send_welcome_email, :_) |> should(be_true())
        end

        it "should mark the welcome email as sent" do
          new_user = Repo.get(User, shared.id)
          new_user.welcome_sent |> should(be_true())
        end

        context "when a valid avatar is passed" do
          before do
            %Metadata{id: id} =
              Factory.insert(
                :tros_metadata,
                user: shared.user,
                access: "public"
              )

            avatar_url = TROS.make_url(id)

            {:ok, avatar_id: id, avatar_url: avatar_url}
          end

          context "and the user does not have an existing avatar" do
            before do
              result = User.update(shared.id, %{avatar: shared.avatar_url})
              {:ok, result: result}
            end

            it "should return :ok" do
              shared.result |> should(be_ok_result())
            end

            it "should update the user's avatar" do
              new_user = Repo.get(User, shared.id)
              new_user.avatar |> should(eq shared.avatar_url)
            end
          end

          context "and the user already has that avatar" do
            before do
              shared.user
              |> cast(%{avatar: shared.avatar_url}, [:avatar])
              |> Repo.update!()

              result = User.update(shared.id, %{avatar: shared.avatar_url})
              {:ok, result: result}
            end

            it "should return :ok" do
              shared.result |> should(be_ok_result())
            end

            it "should not change the user's avatar" do
              new_user = Repo.get(User, shared.id)
              new_user.avatar |> should(eq shared.avatar_url)
            end

            it "should not try to delete the avatar" do
              Metadata.get(shared.avatar_id) |> should_not(be_nil())
            end
          end

          context "and the user has a valid avatar" do
            before do
              avatar_id = ID.new()
              avatar_url = TROS.make_url(avatar_id)
              Metadata.put(avatar_id, shared.user.id, "public")

              shared.user
              |> cast(%{avatar: avatar_url}, [:avatar])
              |> Repo.update!()

              result = User.update(shared.id, %{avatar: shared.avatar_url})
              {:ok, result: result, old_avatar_id: avatar_id}
            end

            it "should return :ok" do
              shared.result |> should(be_ok_result())
            end

            it "should update the user's avatar" do
              new_user = Repo.get(User, shared.id)
              new_user.avatar |> should(eq shared.avatar_url)
            end

            it "should delete the old avatar" do
              Metadata.get(shared.old_avatar_id) |> should(be_nil())
            end
          end
        end
      end

      context "non-indexed user" do
        before do
          user = Factory.insert(:user, %{roles: [User.no_index_role()]})

          fields = %{
            resource: ID.new(),
            handle: Factory.new_handle(),
            first_name: Name.first_name(),
            last_name: Name.last_name(),
            email: Internet.email(),
            tagline: Lorem.sentence()
          }

          User.update(user.id, fields)
          :ok
        end

        it "should not update the index" do
          TestIndexer.get_index_operations() |> should(be_empty())
        end
      end

      context "update to an already-welcomed user" do
        before do
          user = Factory.insert(:user, %{welcome_sent: true})

          fields = %{
            email: Internet.email()
          }

          User.update(user.id, fields)
          :ok
        end

        it "should not re-send the welcome email" do
          :meck.called(Email, :send_welcome_email, :_) |> should(be_false())
        end
      end

      context "update to an un-welcomed user that leaves the email unset" do
        before do
          user = Factory.insert(:user, %{email: nil})

          fields = %{
            first_name: Name.first_name()
          }

          User.update(user.id, fields)
          :ok
        end

        it "should not send a welcome email" do
          :meck.called(Email, :send_welcome_email, :_) |> should(be_false())
        end
      end
    end

    describe "delete/1" do
      before do
        {:ok, _} = Account.assign_token(shared.id, ID.new())
        result = User.delete(shared.id)
        {:ok, result: result}
      end

      it "should return :ok" do
        shared.result |> should(eq :ok)
      end

      it "should remove the user from the database" do
        User |> Repo.get(shared.id) |> should(be_nil())
      end

      it "should succeed if the user does not exist" do
        ID.new() |> User.delete() |> should(eq :ok)
      end

      context "full text search index" do
        it "should be updated" do
          TestIndexer.get_index_operations() |> should_not(be_empty())
        end
      end

      it "should remove any tokens associated with the user" do
        Token |> Repo.get_by(user_id: shared.id) |> should(be_nil())
      end
    end
  end

  describe "location" do
    before do
      Factory.insert_list(5, :location, user_id: shared.id, resource: "test")
      :ok
    end

    it "should return a query for retrieving user locations" do
      query = User.get_locations_query(shared.user, "test")
      query |> Repo.all() |> length() |> should(eq 5)
    end
  end

  describe "add and remove roles" do
    before do
      role = Lorem.word()
      role2 = "role2"
      result = User.add_role(shared.id, role)
      result2 = User.add_role(shared.id, role2)
      {:ok, role: role, role2: role2, result: result, result2: result2}
    end

    describe "add_role/2" do
      it "should add the role to the user" do
        User
        |> Repo.get(shared.id)
        |> Map.get(:roles)
        |> Enum.sort()
        |> should(eq Enum.sort([shared.role, shared.role2]))
      end

      it "should return :ok" do
        shared.result |> should(eq :ok)
        shared.result2 |> should(eq :ok)
      end

      it "should not add re-add the same group" do
        User.add_role(shared.id, shared.role)

        User
        |> Repo.get(shared.id)
        |> Map.get(:roles)
        |> Enum.sort()
        |> should(eq Enum.sort([shared.role, shared.role2]))
      end

      it "should nd not fail when acting on an inavlid user" do
        User.add_role(ID.new(), Lorem.word()) |> should(eq :ok)
      end
    end

    describe "remove_role/2" do
      before do
        result = User.remove_role(shared.id, shared.role)
        {:ok, result: result}
      end

      it "should remove the role form the user" do
        User
        |> Repo.get(shared.id)
        |> Map.get(:roles)
        |> should(eq [shared.role2])
      end

      it "should return :ok" do
        shared.result |> should(eq :ok)
      end

      it "should not have any effect when applied again" do
        User.remove_role(shared.id, shared.role)
        |> should(eq :ok)

        User
        |> Repo.get(shared.id)
        |> Map.get(:roles)
        |> should(eq [shared.role2])
      end

      it "should nd not fail when acting on an inavlid user" do
        User.remove_role(ID.new(), Lorem.word()) |> should(eq :ok)
      end
    end
  end

  describe "bot relationships" do
    before do
      other_user = Factory.insert(:user)

      owned_bot = Factory.insert(:bot, user: shared.user)
      pending_bot = Factory.insert(:bot, user: shared.user, pending: true)
      public_bot = Factory.insert(:bot, user: other_user, public: true)
      shared_bot = Factory.insert(:bot, user: other_user)
      subscribed_bot = Factory.insert(:bot, user: other_user)
      unaffiliated_bot = Factory.insert(:bot, user: other_user)

      Share.put(shared.user, shared_bot, other_user)
      Bot.subscribe(subscribed_bot, shared.user)

      {:ok,
       [
         other_user: other_user,
         owned_bot: owned_bot,
         pending_bot: pending_bot,
         public_bot: public_bot,
         shared_bot: shared_bot,
         subscribed_bot: subscribed_bot,
         unaffiliated_bot: unaffiliated_bot
       ]}
    end

    describe "owns?/2" do
      it do: assert(User.owns?(shared.user, shared.owned_bot))
      it do: refute(User.owns?(shared.user, shared.unaffiliated_bot))
    end

    describe "searchable checks" do
      before do
        friend = Factory.insert(:user)
        RosterHelper.make_friends(friend, shared.user)

        followee = Factory.insert(:user)
        RosterHelper.follow(shared.user, followee)

        friends_public_bot = Factory.insert(:bot, user: friend, public: true)
        friends_private_bot = Factory.insert(:bot, user: friend)
        friends_shared_private_bot = Factory.insert(:bot, user: friend)

        following_public_bot =
          Factory.insert(
            :bot,
            user: followee,
            public: true
          )

        following_private_bot = Factory.insert(:bot, user: followee)
        following_shared_private_bot = Factory.insert(:bot, user: followee)

        Share.put(shared.user, friends_shared_private_bot, friend)
        Share.put(shared.user, following_shared_private_bot, followee)

        {:ok,
         [
           friends_public_bot: friends_public_bot,
           friends_private_bot: friends_private_bot,
           friends_shared_private_bot: friends_shared_private_bot,
           following_public_bot: following_public_bot,
           following_private_bot: following_private_bot,
           following_shared_private_bot: following_shared_private_bot
         ]}
      end

      describe "searchable?/2" do
        it do: assert(User.searchable?(shared.user, shared.subscribed_bot))
        it do: refute(User.searchable?(shared.user, shared.friends_public_bot))

        it do:
             refute(
               User.searchable?(shared.user, shared.friends_shared_private_bot)
             )

        it do:
             refute(User.searchable?(shared.user, shared.following_public_bot))

        it do: refute(User.searchable?(shared.user, shared.public_bot))
        it do: refute(User.searchable?(shared.user, shared.shared_bot))
        it do: refute(User.searchable?(shared.user, shared.unaffiliated_bot))
        it do: refute(User.searchable?(shared.user, shared.friends_private_bot))

        it do:
             refute(User.searchable?(shared.user, shared.following_private_bot))

        it do:
             refute(
               User.searchable?(
                 shared.user,
                 shared.following_shared_private_bot
               )
             )
      end

      describe "searchable stored procedure" do
        it do: assert(is_searchable_sp(shared.user, shared.owned_bot))
        it do: assert(is_searchable_sp(shared.user, shared.subscribed_bot))
        it do: refute(is_searchable_sp(shared.user, shared.friends_public_bot))

        it do:
             refute(
               is_searchable_sp(shared.user, shared.friends_shared_private_bot)
             )

        it do:
             refute(is_searchable_sp(shared.user, shared.following_public_bot))

        it do: refute(is_searchable_sp(shared.user, shared.public_bot))
        it do: refute(is_searchable_sp(shared.user, shared.shared_bot))
        it do: refute(is_searchable_sp(shared.user, shared.unaffiliated_bot))
        it do: refute(is_searchable_sp(shared.user, shared.friends_private_bot))

        it do:
             refute(is_searchable_sp(shared.user, shared.following_private_bot))

        it do:
             refute(
               is_searchable_sp(
                 shared.user,
                 shared.following_shared_private_bot
               )
             )
      end
    end

    describe "can_access?/2" do
      it do: assert(User.can_access?(shared.user, shared.owned_bot))
      it do: assert(User.can_access?(shared.user, shared.shared_bot))
      it do: assert(User.can_access?(shared.user, shared.public_bot))
      it do: refute(User.can_access?(shared.user, shared.unaffiliated_bot))
    end

    describe "get_subscriptions/1" do
      subject do: User.get_subscriptions(shared.user)

      it do: should(have_count 1)
      it do: should(have_any &same_bot(&1, shared.subscribed_bot))
      it do: should_not(have_any &same_bot(&1, shared.owned_bot))
      it do: should_not(have_any &same_bot(&1, shared.pending_bot))
    end

    describe "get_guest_subscriptions/1" do
      before do
        guest = Factory.insert(:bot, user: shared.other_user, geofence: true)

        disabled =
          Factory.insert(:bot, user: shared.other_user, geofence: false)

        Bot.subscribe(guest, shared.user, true)
        Bot.subscribe(disabled, shared.user, true)

        {:ok, guest_bot: guest, disabled_bot: disabled}
      end

      subject do: User.get_guest_subscriptions(shared.user)

      it do: should(have_count 1)
      it do: should(have_any &same_bot(&1, shared.guest_bot))
      it do: should_not(have_any &same_bot(&1, shared.disabled_bot))
      it do: should_not(have_any &same_bot(&1, shared.subscribed_bot))
      it do: should_not(have_any &same_bot(&1, shared.owned_bot))
      it do: should_not(have_any &same_bot(&1, shared.pending_bot))
    end

    describe "bot_count/1" do
      it do: User.bot_count(shared.user) |> should(eq 1)
      it do: User.bot_count(shared.other_user) |> should(eq 4)
    end

    describe "get_owned_bots/1" do
      subject do: User.get_owned_bots(shared.user)

      it do: should(have_count 1)
      it do: should(have_any &same_bot(&1, shared.owned_bot))
      it do: should_not(have_any &same_bot(&1, shared.pending_bot))
    end
  end

  describe "is_visible_query/2" do
    before do
      user1 = shared.user
      user2 = Factory.insert(:user)
      owned_bot = Factory.insert(:bot, user: user1)
      public_bot = Factory.insert(:bot, user: user2, public: true)
      shared_bot = Factory.insert(:bot, user: user2)
      Factory.insert(:share, user: user1, bot: shared_bot, sharer: user2)
      private_bot = Factory.insert(:bot, user: user2)
      pending_bot = Factory.insert(:bot, user: user1, pending: true)

      {:ok,
       owned_bot: owned_bot,
       public_bot: public_bot,
       shared_bot: shared_bot,
       private_bot: private_bot,
       pending_bot: pending_bot}
    end

    it "should allow owned bots" do
      is_visible_sp(shared.user, shared.owned_bot)
      |> should(be_true())
    end

    it "should allow public bots" do
      is_visible_sp(shared.user, shared.public_bot)
      |> should(be_true())
    end

    it "should allow shared bots" do
      is_visible_sp(shared.user, shared.shared_bot)
      |> should(be_true())
    end

    it "should refuse private bots" do
      is_visible_sp(shared.user, shared.private_bot)
      |> should(be_false())
    end
  end

  describe "avatar deletion", async: false do
    before do
      user = Factory.insert(:user, avatar: nil)
      avatar = Factory.insert(:tros_metadata, user: user)

      {:ok, user: user, avatar: avatar, avatar_url: TROS.make_url(avatar.id)}
    end

    context "when no avatar is set" do
      it "should not delete the avatar when a new one is set" do
        User.update(shared.user.id, %{avatar: shared.avatar_url})
        |> should(be_ok_result())

        Metadata.get(shared.avatar.id) |> should_not(be_nil())
      end

      it "should not delete the avatar when a new one is set" do
        User.update(shared.user.id, %{first_name: Name.first_name()})
        |> should(be_ok_result())

        Metadata.get(shared.avatar.id) |> should_not(be_nil())
      end

      it "should not delete the avatar when the same one is set" do
        User.update(shared.user.id, %{avatar: shared.user.avatar})
        |> should(be_ok_result())

        Metadata.get(shared.avatar.id) |> should_not(be_nil())
      end
    end

    context "when an avatar is set" do
      before do
        User.update(shared.user.id, %{avatar: shared.avatar_url})
        new_avatar = Factory.insert(:tros_metadata, user: shared.user)

        {:ok,
         new_avatar: new_avatar, new_avatar_url: TROS.make_url(new_avatar.id)}
      end

      it "should delete the avatar when a new one is set" do
        User.update(shared.user.id, %{avatar: shared.new_avatar_url})
        |> should(be_ok_result())

        Metadata.get(shared.avatar.id) |> should(be_nil())
      end

      it "should not delete the avatar when one is not set" do
        User.update(shared.user.id, %{first_name: Name.first_name()})
        |> should(be_ok_result())

        Metadata.get(shared.avatar.id) |> should_not(be_nil())
      end

      it "should not delete the avatar when the same one is set" do
        User.update(shared.user.id, %{avatar: shared.user.avatar})
        |> should(be_ok_result())

        Metadata.get(shared.avatar.id) |> should_not(be_nil())
      end
    end
  end

  describe "full_name/1" do
    it do: User.full_name(shared.user) |> should(be_binary())
  end

  describe "remove_auth_details/1" do
    context "valid user" do
      before do
        {:ok, result: User.remove_auth_details(shared.user.id)}
      end

      it "should return :ok", do: shared.result |> should(eq :ok)

      it "should clear the user's auth details" do
        user = Repo.get(User, shared.user.id)
        user.phone_number |> should(eq nil)
        user.provider |> should(eq nil)
        user.external_id |> should(eq nil)
      end
    end

    context "invalid user" do
      before do
        {:ok, result: User.remove_auth_details(ID.new())}
      end

      it "should return :ok", do: shared.result |> should(eq :ok)
    end
  end

  describe "search_by_name/3" do
    before do
      users =
        [
          {"Alice", "Sanders", "Xena"},
          {"Alison", "Smith", "Yaniv"},
          {"Bob", "Jones", "Zena"},
          {"acéñtîâ", "CAPITAL", "1345"}
        ]
        |> Enum.map(fn {f, l, h} ->
          Factory.insert(:user, first_name: f, last_name: l, handle: h)
        end)

      {:ok, users: users}
    end

    context "no blocking" do
      it "should return all users with the search prefix in either name" do
        User.search_by_name("a", shared.id, 50) |> should(have_length(3))
        User.search_by_name("b", shared.id, 50) |> should(have_length(1))
        User.search_by_name("s", shared.id, 50) |> should(have_length(2))
        User.search_by_name("smi", shared.id, 50) |> should(have_length(1))
        User.search_by_name("q", shared.id, 50) |> should(have_length(0))
        User.search_by_name("z", shared.id, 50) |> should(have_length(1))
        User.search_by_name("13", shared.id, 50) |> should(have_length(1))
      end

      it "should ignore accents in both search and data" do
        User.search_by_name("acent", shared.id, 50) |> should(have_length(1))
        User.search_by_name("â", shared.id, 50) |> should(have_length(3))
      end

      it "should ignore capitalisation in both search and data" do
        User.search_by_name("A", shared.id, 50) |> should(have_length(3))
        User.search_by_name("c", shared.id, 50) |> should(have_length(1))
      end

      it "should respect the limit parameter" do
        User.search_by_name("a", shared.id, 2) |> should(have_length(2))
      end

      it "should ignore empty search terms and return an empty list" do
        User.search_by_name("", shared.id, 50) |> should(have_length(0))
      end

      it "should work on multiple partial terms" do
        User.search_by_name("ali s", shared.id, 50) |> should(have_length(2))
        User.search_by_name("ali sm", shared.id, 50) |> should(have_length(1))
      end

      it "should not choke on punctuation or other unicode weirdness" do
        User.search_by_name("''ali", shared.id, 50) |> should(have_length(2))
        User.search_by_name("al-s", shared.id, 50) |> should(have_length(0))
        User.search_by_name("al''i", shared.id, 50) |> should(have_length(2))
        User.search_by_name("al''i", shared.id, 50) |> should(have_length(2))
        User.search_by_name("''-al''i", shared.id, 50) |> should(have_length(2))
      end
    end

    context "when the searcher is blocked" do
      before do
        # Alice Sanders
        blocking_user = hd(shared.users)
        Block.block(blocking_user, shared.user)
        result = User.search_by_name("a", shared.id, 50)
        {:ok, blocking_user: blocking_user, result: result}
      end

      it "should not return the blocking user" do
        Enum.any?(shared.result, fn %{id: id} ->
          id == shared.blocking_user.id
        end)
        |> should(be_false())

        shared.result |> should(have_length(2))
      end
    end
  end

  defp same_bot(bot1, bot2), do: bot1.id == bot2.id

  defp is_searchable_sp(user, bot),
    do: run_stored_proc(user, bot, "is_searchable")

  defp is_visible_sp(user, bot), do: run_stored_proc(user, bot, "is_visible")

  defp run_stored_proc(user, bot, proc) do
    {:ok, u} = Ecto.UUID.dump(user.id)
    {:ok, b} = Ecto.UUID.dump(bot.id)
    Repo

    result =
      SQL.query!(
        Repo,
        "SELECT * FROM bots AS bot WHERE id = $2 AND #{proc}($1, bot)",
        [u, b]
      )

    length(result.rows) == 1
  end

  defp set_handle(shared, handle) do
    shared.user
    |> User.changeset(%{handle: handle})
  end

  defp set_name(shared, name) do
    field = Enum.random([:first_name, :last_name])

    shared.user
    |> User.changeset(%{field => name})
  end
end
