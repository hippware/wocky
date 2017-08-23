defmodule Wocky.UserSpec do
  use ESpec, async: true
  use ModelHelpers
  use Wocky.JID

  alias Ecto.Adapters.SQL
  alias Faker.Internet
  alias Faker.Lorem
  alias Timex.Duration
  alias Wocky.Bot.Share
  alias Wocky.Bot.Subscription
  alias Wocky.Bot.TempSubscription
  alias Wocky.Index.TestIndexer
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.Token
  alias Wocky.TROS
  alias Wocky.TROS.Metadata
  alias Wocky.User

  before do
    user = Factory.insert(:user, %{resource: "testing"})
    {:ok, user: user, id: user.id, external_id: user.external_id}
  end

  describe "valid_update_fields/0" do
    subject do: User.valid_update_fields

    it do: should(have_count 7)
  end

  describe "to_jid/1" do
    it "should return the user's JID" do
      jid1 = User.to_jid(shared.user)
      jid2 = JID.make(shared.user.id, shared.user.server, shared.user.resource)
      jid1 |> JID.equal?(jid2) |> should(be_true())
    end
  end

  describe "get_by_jid/1" do
    context "when the user exists" do
      subject do: shared.user |> User.to_jid |> User.get_by_jid

      it do: subject().id |> should(eq shared.user.id)
      it do: subject().resource |> should(eq shared.user.resource)
    end

    context "when the user does not exist" do
      subject do: ID.new |> JID.make(shared.server) |> User.get_by_jid

      it do: should(be_nil())
    end
  end

  describe "register_changeset/1 validations" do
    it "should pass with valid attributes" do
      %{username: ID.new, server: "foo", provider: "local", external_id: "bar"}
      |> User.register_changeset
      |> should(be_valid())
    end

    it "should fail if the username is missing" do
      %{server: "foo", eternal_id: "bar"}
      |> User.register_changeset
      |> should(have_errors([:username]))
    end

    it "should fail with an invalid username" do
      %{username: "alice", server: "foo", external_id: "bar"}
      |> User.register_changeset
      |> should(have_errors([:username]))
    end

    it "should fail if the server is missing" do
      %{username: ID.new, eternal_id: "bar"}
      |> User.register_changeset
      |> should(have_errors([:server]))
    end

    it "should fail if the external ID is missing" do
      %{username: ID.new, server: "foo"}
      |> User.register_changeset
      |> should(have_errors([:external_id]))
    end

    it "should set the ID to the username" do
      id = ID.new

      changeset = User.register_changeset(%{
            username: id,
            server: "foo",
            eternal_id: "bar"
      })

      changeset.changes.id |> should(eq changeset.changes.username)
      changeset.changes.id |> should(eq id)
    end
  end

  describe "register_external/4" do
    context "when the user already exists" do
      before do
        {:ok, result} =
          User.register_external("another_server", "firebase",
                                 shared.external_id, "+15551234567")

        {:shared, result: result}
      end

      it "should return the ID of the existing user" do
        {result_id, _, _} = shared.result
        result_id |> should(eq shared.id)
      end

      it "should return the server of the existing user" do
        {_, result_server, _} = shared.result
        result_server |> should(eq shared.server)
        result_server |> should_not(eq "another_server")
      end

      it "should return 'false' in the last slot" do
        {_, _, result_is_new} = shared.result
        result_is_new |> should_not(be_true())
      end
    end

    context "when the user does not exist" do
      before do
        {:ok, result} =
          User.register_external(shared.server, "firebase",
                                 ID.new, "+15551234567")

        {:shared, result: result}
      end

      finally do
        {id, _, _} = shared.result
        Repo.delete!(%User{id: id})
      end

      it "should create the user and return its ID" do
        {result_id, _, _} = shared.result
        obj = Repo.get(User, result_id)
        obj |> should_not(be_nil())
      end

      it "should return the server that was passed in" do
        {_, result_server, _} = shared.result
        result_server |> should(eq shared.server)
      end

      it "should return 'true' in the last slot" do
        {_, _, result_is_new} = shared.result
        result_is_new |> should(be_true())
      end
    end
  end

  describe "register/4" do
    before do
      id = ID.new
      result = User.register(id, shared.server, "password", "password")
      {:ok, id: id, result: result}
    end

    it "should return a success result" do
      shared.result |> should(be_ok_result())
    end

    it "should create a user" do
      new_user = Repo.get(User, shared.id)

      new_user |> should_not(be_nil())
      new_user.server |> should(eq shared.server)
      new_user.password |> should(eq "password")
      new_user.pass_details |> should(eq "password")
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
      shared.user
      |> User.changeset(%{handle: "Root"})
      |> should(have_errors([:handle]))
    end

    context "when there is a pre-existing matching handle regardless of case" do
      subject do
        :user
        |> Factory.insert
        |> User.changeset(%{handle: String.upcase(shared.user.handle)})
        |> Repo.update
      end

      it do: should(be_error_result())
      it do: subject() |> Kernel.elem(1) |> should(have_errors([:handle]))
    end

    context "avatar validations" do
      context "with invalid data" do
        before do
          file_id = ID.new
          url = TROS.make_url(shared.server, file_id)
          Metadata.put(file_id, ID.new, "public")
          {:ok, file_id: file_id, url: url}
        end

        finally do
          Metadata.delete(shared.file_id)
        end

        it "should fail with an invalid avatar URL" do
          shared.user
          |> User.changeset(%{avatar: "not a valid URL"})
          |> should(have_errors([:avatar]))
        end

        it "should fail with a non-existing avatar URL" do
          shared.user
          |> User.changeset(%{avatar: TROS.make_url(shared.server, ID.new)})
          |> should(have_errors([:avatar]))
        end

        it "should fail with a non-local avatar URL" do
          shared.user
          |> User.changeset(%{avatar: TROS.make_url("otherhost", ID.new)})
          |> should(have_errors([:avatar]))
        end

        it "should fail with a file owned by another user" do
          shared.user
          |> User.changeset(%{avatar: shared.url})
          |> should(have_errors([:avatar]))
        end
      end
    end

    context "with valid data" do
      before do
        file_id = ID.new
        url = TROS.make_url(shared.server, file_id)
        Metadata.put(file_id, shared.user.id, "public")
        {:ok, file_id: file_id, url: url}
      end

      finally do
        Metadata.delete(shared.file_id)
      end

      it "should be valid" do
        shared.user
        |> User.changeset(%{avatar: shared.url})
        |> should(be_valid())
      end
    end
  end

  describe "tests using TestIndexer", async: false do
    before do
      TestIndexer.reset
    end

    describe "update/2" do
      before do
        fields = %{
          resource: ID.new,
          handle: Internet.user_name,
          first_name: "Svein",
          last_name: "Forkbeard",
          email: "svein@forkbeard.com",
          tagline: "It was the blurst of times"
        }

        result = User.update(shared.id, fields)
        {:ok, fields: fields, result: result}
      end

      it "should return :ok" do
        shared.result |> should(eq :ok)
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
          TestIndexer.get_index_operations |> should_not(be_empty())
        end
      end

      context "when a valid avatar is passed" do
        before do
          avatar_id = ID.new
          avatar_url = TROS.make_url(shared.server, avatar_id)

          Metadata.put(avatar_id, shared.user.id, "public")

          {:ok, avatar_id: avatar_id, avatar_url: avatar_url}
        end

        finally do
          Metadata.delete(shared.avatar_id)
        end

        context "and the user does not have an existing avatar" do
          before do
            result = User.update(shared.id, %{avatar: shared.avatar_url})
            {:ok, result: result}
          end

          it "should return :ok" do
            shared.result |> should(eq :ok)
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
            |> Repo.update!

            result = User.update(shared.id, %{avatar: shared.avatar_url})
            {:ok, result: result}
          end

          it "should return :ok" do
            shared.result |> should(eq :ok)
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
            avatar_id = ID.new
            avatar_url = TROS.make_url(shared.server, avatar_id)
            Metadata.put(avatar_id, shared.user.id, "public")

            shared.user
            |> cast(%{avatar: avatar_url}, [:avatar])
            |> Repo.update!

            result = User.update(shared.id, %{avatar: shared.avatar_url})
            {:ok, result: result, old_avatar_id: avatar_id}
          end

          it "should return :ok" do
            shared.result |> should(eq :ok)
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

    describe "delete/1" do
      before do
        {:ok, _} = Token.assign(shared.id, ID.new)
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
        ID.new |> User.delete |> should(eq :ok)
      end

      context "full text search index" do
        it "should be updated" do
          TestIndexer.get_index_operations |> should_not(be_empty())
        end
      end

      it "should remove any tokens associated with the user" do
        Token |> Repo.get_by(user_id: shared.id) |> should(be_nil())
      end
    end
  end

  describe "set_location/5" do

  end

  describe "add and remove roles" do
    before do
      role = Lorem.word
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
        |> Enum.sort
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
        |> Enum.sort
        |> should(eq Enum.sort([shared.role, shared.role2]))
      end

      it "should nd not fail when acting on an inavlid user" do
        User.add_role(ID.new, Lorem.word) |> should(eq :ok)
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
        User.remove_role(ID.new, Lorem.word) |> should(eq :ok)
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
      temp_subscribed_bot = Factory.insert(:bot, user: other_user)
      unaffiliated_bot = Factory.insert(:bot, user: other_user)

      Share.put(shared.user, shared_bot, other_user)
      Subscription.put(shared.user, subscribed_bot)
      TempSubscription.put(shared.user, temp_subscribed_bot, node())

      {:ok, [
          other_user: other_user,
          owned_bot: owned_bot,
          pending_bot: pending_bot,
          public_bot: public_bot,
          shared_bot: shared_bot,
          subscribed_bot: subscribed_bot,
          temp_subscribed_bot: temp_subscribed_bot,
          unaffiliated_bot: unaffiliated_bot,
        ]}
    end

    describe "owns?/2" do
      it do: assert User.owns?(shared.user, shared.owned_bot)
      it do: refute User.owns?(shared.user, shared.unaffiliated_bot)
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
        following_public_bot = Factory.insert(:bot, user: followee,
                                              public: true)
        following_private_bot = Factory.insert(:bot, user: followee)
        following_shared_private_bot = Factory.insert(:bot, user: followee)

        Share.put(shared.user, friends_shared_private_bot, friend)
        Share.put(shared.user, following_shared_private_bot, followee)

        {:ok, [
          friends_public_bot: friends_public_bot,
          friends_private_bot: friends_private_bot,
          friends_shared_private_bot: friends_shared_private_bot,
          following_public_bot: following_public_bot,
          following_private_bot: following_private_bot,
          following_shared_private_bot: following_shared_private_bot
        ]}
      end

      describe "searchable?/2" do
        it do: assert User.searchable?(shared.user, shared.owned_bot)
        it do: assert User.searchable?(shared.user, shared.subscribed_bot)
        it do: assert User.searchable?(shared.user, shared.friends_public_bot)
        it do: assert User.searchable?(shared.user,
                                       shared.friends_shared_private_bot)
        it do: assert User.searchable?(shared.user, shared.following_public_bot)

        it do: refute User.searchable?(shared.user, shared.temp_subscribed_bot)
        it do: refute User.searchable?(shared.user, shared.public_bot)
        it do: refute User.searchable?(shared.user, shared.shared_bot)
        it do: refute User.searchable?(shared.user, shared.unaffiliated_bot)
        it do: refute User.searchable?(shared.user, shared.friends_private_bot)
        it do: refute User.searchable?(shared.user, shared.following_private_bot)
        it do: refute User.searchable?(shared.user,
                                       shared.following_shared_private_bot)
      end

      describe "searchable stored procedure" do
        it do: assert is_searchable_sp(shared.user, shared.owned_bot)
        it do: assert is_searchable_sp(shared.user, shared.subscribed_bot)
        it do: assert is_searchable_sp(shared.user, shared.friends_public_bot)
        it do: assert is_searchable_sp(shared.user,
                                       shared.friends_shared_private_bot)
        it do: assert is_searchable_sp(shared.user, shared.following_public_bot)

        it do: refute is_searchable_sp(shared.user, shared.temp_subscribed_bot)
        it do: refute is_searchable_sp(shared.user, shared.public_bot)
        it do: refute is_searchable_sp(shared.user, shared.shared_bot)
        it do: refute is_searchable_sp(shared.user, shared.unaffiliated_bot)
        it do: refute is_searchable_sp(shared.user, shared.friends_private_bot)
        it do: refute is_searchable_sp(shared.user, shared.following_private_bot)
        it do: refute is_searchable_sp(shared.user,
                                       shared.following_shared_private_bot)
      end
    end

    describe "can_access?/2" do
      it do: assert User.can_access?(shared.user, shared.owned_bot)
      it do: assert User.can_access?(shared.user, shared.shared_bot)
      it do: assert User.can_access?(shared.user, shared.public_bot)
      it do: refute User.can_access?(shared.user, shared.unaffiliated_bot)
    end

    describe "subscribed?/2" do
      it do: assert User.subscribed?(shared.user, shared.owned_bot)
      it do: assert User.subscribed?(shared.user, shared.subscribed_bot)
      it do: assert User.subscribed?(shared.user, shared.temp_subscribed_bot)
      it do: refute User.subscribed?(shared.user, shared.unaffiliated_bot)
    end

    describe "get_subscriptions/1" do
      subject do: User.get_subscriptions(shared.user)

      it do: should(have_count 3)
      it do: should(have_any &same_bot(&1, shared.owned_bot))
      it do: should(have_any &same_bot(&1, shared.subscribed_bot))
      it do: should(have_any &same_bot(&1, shared.temp_subscribed_bot))
      it do: should_not(have_any &same_bot(&1, shared.pending_bot))
    end

    describe "bot_count/1" do
      it do: User.bot_count(shared.user) |> should(eq 1)
      it do: User.bot_count(shared.other_user) |> should(eq 5)
    end

    describe "get_owned_bots/1" do
      subject do: User.get_owned_bots(shared.user)

      it do: should(have_count 1)
      it do: should(have_any &same_bot(&1, shared.owned_bot))
      it do: should_not(have_any &same_bot(&1, shared.pending_bot))
    end

    describe "get_owned_bots_with_follow_me/1" do
      before do
        follow_bot = Factory.insert(:bot, [
              user: shared.user,
              follow_me: true,
              follow_me_expiry:
                Timex.add(DateTime.utc_now, Duration.from_seconds(1000))
            ])

        {:ok, follow_bot: follow_bot}
      end

      subject do: User.get_owned_bots_with_follow_me(shared.user)

      it do: should(have_count 1)
      it do: should(have_any &same_bot(&1, shared.follow_bot))
      it do: should_not(have_any &same_bot(&1, shared.owned_bot))
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
        pending_bot: pending_bot
      }
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


  defp same_bot(bot1, bot2), do: bot1.id == bot2.id

  defp is_searchable_sp(user, bot),
    do: run_stored_proc(user, bot, "is_searchable")
  defp is_visible_sp(user, bot),
    do: run_stored_proc(user, bot, "is_visible")

  defp run_stored_proc(user, bot, proc) do
    {:ok, u} = Ecto.UUID.dump(user.id)
    {:ok, b} = Ecto.UUID.dump(bot.id)
    Repo
    result = SQL.query!(
      Repo,
      "SELECT * FROM bots AS bot WHERE id = $2 AND #{proc}($1, bot)",
      [u, b])
    length(result.rows) == 1
  end
end
