defmodule Wocky.UserSpec do
  use ESpec, async: true
  use ModelHelpers
  use Wocky.JID

  alias Faker.Internet
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.Token
  alias Wocky.TROS
  alias Wocky.TROS.Metadata
  alias Wocky.User

  before do
    user = Factory.insert(:user, %{server: shared.server})
    {:ok, user: user, id: user.id, external_id: user.external_id}
  end

  describe "register_changeset/1 validations" do
    it "should pass with valid attributes" do
      %{username: ID.new, server: "foo", external_id: "bar"}
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

  describe "register/3" do
    context "when the user already exists" do
      before do
        {:ok, result} =
          User.register("another_server", shared.external_id, "+15551234567")

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
          User.register(shared.server, ID.new, "+15551234567")

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
      new_user = User.find(shared.id)

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

  describe "update/2" do
    before do
      fields = %{
        resource: ID.new,
        handle: Internet.user_name,
        first_name: "Svein",
        last_name: "Forkbeard",
        email: "svein@forkbeard.com"
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
    end

    it "should not update the user's resource" do
      new_user = Repo.get(User, shared.id)
      new_user.resource |> should(be_nil())
    end

    it "should update the user's entry in the full text search index"

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
          new_user = User.find(shared.id)
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
          new_user = User.find(shared.id)
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
          new_user = User.find(shared.id)
          new_user.avatar |> should(eq shared.avatar_url)
        end

        it "should delete the old avatar" do
          Metadata.get(shared.old_avatar_id) |> should(be_nil())
        end
      end
    end
  end

  describe "set_location/5" do

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

    it "should remove any tokens associated with the user" do
      shared.id
      |> Token.get_all
      |> should(be_empty())
    end

    it "should remove any location data associated with the user"

    it "should remove the user from the full text search index"

    it "should succeed if the user does not exist" do
      ID.new
      |> User.delete
      |> should(eq :ok)
    end
  end

  describe "find/2" do
    context "when the user exists" do
      before do
        result = User.find(shared.id)
        {:ok, result: result}
      end

      it "should return a User struct" do
        shared.result |> should(be_struct User)
      end

      it "should return the right user" do
        shared.result.id |> should(eq shared.id)
        shared.result.server |> should(eq shared.server)
        shared.result.external_id |> should(eq shared.external_id)
      end
    end

    context "when the user does not exist" do
      it "should return `nil`" do
        ID.new
        |> User.find
        |> should(be_nil())
      end
    end
  end

  describe "find_by/2" do
    context "when there is a matching user" do
      before do
        result = User.find_by(:external_id, shared.external_id)
        {:ok, result: result}
      end

      it "should return a User struct" do
        shared.result |> should(be_struct User)
      end

      it "should return matching user" do
        shared.result |> should(eq User.find(shared.id))
      end
    end

    context "when there is not a matching user" do
      before do
        result = User.find_by(:external_id, "nosuchuser")
        {:ok, result: result}
      end

      it "should return nil" do
        shared.result |> should(be_nil())
      end
    end
  end

  describe "get_handle/1" do
    it "should return the user's handle" do
      shared.id |> User.get_handle |> should(eq shared.user.handle)
    end

    it "should return nil if the user does not exist" do
      ID.new |> User.get_handle |> should(eq nil)
    end
  end

  describe "get_phone_number/1" do
    it "should return the user's phone number" do
      shared.id |> User.get_phone_number |> should(eq shared.user.phone_number)
    end

    it "should return nil if the user does not exist" do
      ID.new |> User.get_phone_number |> should(eq nil)
    end
  end

  describe "get_subscribed_bots/1" do

  end

  describe "get_owned_bots/1" do

  end

  describe "get_owned_bots_with_follow_me/1" do

  end

  describe "to_jid/1" do
    it "should return the user's JID" do
      jid1 = User.to_jid(shared.user)
      jid2 = JID.make(shared.user.id, shared.user.server)
      jid1 |> JID.equal?(jid2) |> should(be_true())
    end
  end
end
