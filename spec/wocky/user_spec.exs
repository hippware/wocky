defmodule Wocky.UserSpec do
  use ESpec, async: true

  alias Faker.Internet
  alias Wocky.ID
  alias Wocky.Repo
  alias Wocky.User
  alias Wocky.User.Token

  before do
    id = ID.new
    external_id = ID.new
    :ok = User.update(id, shared.server, %{external_id: external_id})
    {:ok, id: id, external_id: external_id}
  end

  finally do
    User.delete(shared.server, shared.id)
  end

  describe "new/3" do
    before do
      id = ID.new
      user = User.new(id, shared.server, %{resource: "testing"})
      {:ok, id: id, user: user}
    end

    it "should return a User struct" do
      shared.user |> should(be_struct User)
    end

    it "should include the passed id" do
      shared.user.id |> should(eq shared.id)
    end

    it "should include the passed server" do
      shared.user.server |> should(eq shared.server)
    end

    it "should include passed data in struct" do
      shared.user.resource |> should(eq "testing")
    end
  end

  describe "register/3" do
    context "when the user already exists" do
      before do
        :ok = User.wait_for_user(shared.id)

        {:ok, result} =
          User.register("another_server", shared.external_id, "+15551234567")

        {:shared, result: result}
      end

      it "returns the ID of the existing user" do
        {result_id, _, _} = shared.result
        result_id |> should(eq shared.id)
      end

      it "returns the server of the existing user" do
        {_, result_server, _} = shared.result
        result_server |> should(eq shared.server)
        result_server |> should_not(eq "another_server")
      end

      it "returns 'false' in the last slot" do
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
        User.delete(shared.server, id)
      end

      it "creates the user and returns its ID" do
        {result_id, _, _} = shared.result
        obj = Repo.find("users", shared.server, result_id)
        obj |> should_not(be_nil())
      end

      it "returns the server that was passed in" do
        {_, result_server, _} = shared.result
        result_server |> should(eq shared.server)
      end

      it "returns 'true' in the last slot" do
        {_, _, result_is_new} = shared.result
        result_is_new |> should(be_true())
      end
    end
  end

  describe "wait_for_user/3" do
    it "should return :ok if the user is in the index" do
      shared.id |> User.wait_for_user |> should(eq :ok)
    end

    it "should return {:error, :timeout} if the user doesn't show up" do
      ID.new |> User.wait_for_user(100, 1) |> should(be_error_result())
    end
  end

  describe "update/1,3" do
    before do
      user = %User{
        id: shared.id,
        server: shared.server,
        resource: ID.new,
        handle: Internet.user_name,
        first_name: "Svein",
        last_name: "Forkbeard",
        email: "svein@forkbeard.com"
      }

      result = User.update(user)
      {:ok, user: user, result: result}
    end

    it "returns :ok" do
      shared.result |> should(eq :ok)
    end

    it "updates the user's attributes" do
      new_user = User.find(shared.id, shared.server)
      new_user.handle |> should(eq shared.user.handle)
      new_user.first_name |> should(eq shared.user.first_name)
      new_user.last_name |> should(eq shared.user.last_name)
      new_user.email |> should(eq shared.user.email)
    end

    it "does not update the user's resource" do
      new_user = User.find(shared.id, shared.server)
      new_user.resource |> should(be_nil())
    end

    context "when passed a handle the same as the old" do
      before do
        result = User.update(shared.id, shared.server,
                             %{handle: shared.user.handle})
        {:ok, result: result}
      end

      it "returns :ok" do
        shared.result |> should(eq :ok)
      end

      it "does not change the user's handle" do
        new_user = User.find(shared.id, shared.server)
        new_user.handle |> should(eq shared.user.handle)
      end
    end

    context "when passed a handle that already exists" do
      before do
        :ok = User.wait_for_user(shared.id)

        new_id = ID.new
        new_user = %User{
          id: new_id,
          server: shared.server,
          handle: shared.user.handle
        }

        result = User.update(new_user)
        {:ok, new_id: new_id, result: result}
      end

      it "returns an error" do
        shared.result |> should(be_error_result())
        shared.result |> should(eq {:error, :duplicate_handle})
      end

      it "does not update the user's attributes" do
        shared.new_id |> User.find(shared.server) |> should(be_nil())
      end
    end

    context "when passed a reserved handle" do
      before do
        result = User.update(shared.id, shared.server,
                             %{handle: "root"})
        {:ok, result: result}
      end

      it "returns an error" do
        shared.result |> should(be_error_result())
        shared.result |> should(eq {:error, :duplicate_handle})
      end

      it "does not update the user's attributes" do
        new_user = User.find(shared.id, shared.server)
        new_user.handle |> should(eq shared.user.handle)
      end
    end

    context "when passed an empty list of fields" do
      before do
        result = User.update(shared.id, shared.server, %{})
        {:ok, result: result}
      end

      it "returns :ok" do
        shared.result |> should(eq :ok)
      end

      it "does not update the user's attributes" do
        new_user = User.find(shared.id, shared.server)
        new_user.handle |> should(eq shared.user.handle)
        new_user.first_name |> should(eq shared.user.first_name)
        new_user.last_name |> should(eq shared.user.last_name)
        new_user.email |> should(eq shared.user.email)
      end
    end

    context "when a valid avatar is passed", async: false do
      before do
        server = shared.server
        avatar_id = ID.new
        avatar_url = :tros.make_url(shared.server, avatar_id)

        allow :tros |> to(accept get_metadata: fn ^server, ^avatar_id ->
                                   {:ok, %{owner: shared.id}}
                                 end,
                                 delete: fn _, _ ->
                                   :ok
                                 end)

        {:ok, avatar_id: avatar_id, avatar_url: avatar_url}
      end

      context "and the user does not have an existing avatar" do
        before do
          result = User.update(shared.id, shared.server,
                               %{avatar: shared.avatar_url})

          {:ok, result: result}
        end

        it "returns :ok" do
          shared.result |> should(eq :ok)
        end

        it "updates the user's avatar" do
          new_user = User.find(shared.id, shared.server)
          new_user.avatar |> should(eq shared.avatar_url)
        end

        it "does not try to delete the old avatar" do
          :tros |> should_not(accepted :delete)
        end
      end

      context "and the user has an invalid avatar" do
        before do
          :ok = Repo.update(%{avatar: "bogus"}, "users",
                            shared.server, shared.id)

          result = User.update(shared.id, shared.server,
                               %{avatar: shared.avatar_url})

          {:ok, result: result}
        end

        it "returns :ok" do
          shared.result |> should(eq :ok)
        end

        it "updates the user's avatar" do
          new_user = User.find(shared.id, shared.server)
          new_user.avatar |> should(eq shared.avatar_url)
        end

        it "does not try to delete the old avatar" do
          :tros |> should_not(accepted :delete)
        end
      end

      context "and the user already has that avatar" do
        before do
          :ok = Repo.update(%{avatar: shared.avatar_url}, "users",
                            shared.server, shared.id)

          result = User.update(shared.id, shared.server,
                               %{avatar: shared.avatar_url})

          {:ok, result: result}
        end

        it "returns :ok" do
          shared.result |> should(eq :ok)
        end

        it "does not change the user's avatar" do
          new_user = User.find(shared.id, shared.server)
          new_user.avatar |> should(eq shared.avatar_url)
        end

        it "does not try to delete the avatar" do
          :tros |> should_not(accepted :delete)
        end
      end

      context "and the user has a valid avatar" do
        before do
          avatar_url = :tros.make_url(shared.server, ID.new)
          :ok = Repo.update(%{avatar: avatar_url}, "users",
                            shared.server, shared.id)

          result = User.update(shared.id, shared.server,
                               %{avatar: shared.avatar_url})

          {:ok, result: result}
        end

        it "returns :ok" do
          shared.result |> should(eq :ok)
        end

        it "updates the user's avatar" do
          new_user = User.find(shared.id, shared.server)
          new_user.avatar |> should(eq shared.avatar_url)
        end

        it "deletes the old avatar" do
          :tros |> should(accepted :delete)
        end
      end
    end

    context "when an avatar with a different owner is passed", async: false do
      before do
        server = shared.server
        avatar_id = ID.new

        allow :tros |> to(accept get_metadata: fn ^server, ^avatar_id ->
                                   {:ok, %{owner: ID.new}}
                                 end)

        avatar_url = :tros.make_url(shared.server, avatar_id)
        result = User.update(shared.id, shared.server,
                             %{avatar: avatar_url})

        {:ok, result: result}
      end

      it "returns an error" do
        shared.result |> should(be_error_result())
        shared.result |> should(eq {:error, :not_file_owner})
      end

      it "does not update the user's avatar" do
        new_user = User.find(shared.id, shared.server)
        new_user.avatar |> should(be_nil())
      end
    end

    context "when a non-local avatar is passed" do
      before do
        avatar_url = :tros.make_url("nonlocal.com", ID.new)
        result = User.update(shared.id, shared.server,
                             %{avatar: avatar_url})

        {:ok, result: result}
      end

      it "returns an error" do
        shared.result |> should(be_error_result())
        shared.result |> should(eq {:error, :not_local_file})
      end

      it "does not update the user's avatar" do
        new_user = User.find(shared.id, shared.server)
        new_user.avatar |> should(be_nil())
      end
    end

    context "when a non-existing avatar is passed", async: false do
      before do
        server = shared.server
        avatar_id = ID.new

        allow :tros |> to(accept get_metadata: fn ^server, ^avatar_id ->
                                   {:error, :not_found}
                                 end)

        avatar_url = :tros.make_url(shared.server, avatar_id)
        result = User.update(shared.id, shared.server,
                             %{avatar: avatar_url})

        {:ok, result: result}
      end

      it "returns an error" do
        shared.result |> should(be_error_result())
        shared.result |> should(eq {:error, :not_found})
      end

      it "does not update the user's avatar" do
        new_user = User.find(shared.id, shared.server)
        new_user.avatar |> should(be_nil())
      end
    end
  end

  describe "delete/2" do
    before do
      {:ok, _} = Token.assign(shared.id, shared.server, ID.new)
      result = User.delete(shared.id, shared.server)
      {:ok, result: result}
    end

    it "should return :ok" do
      shared.result |> should(eq :ok)
    end

    it "should remove the user from the database" do
      shared.server |> User.find(shared.id) |> should(be_nil())
    end

    it "should remove any tokens associated with the user" do
      shared.id
      |> Token.get_tokens(shared.server)
      |> should(be_empty())
    end

    it "should remove any location data associated with the user"

    it "should succeed if the user does not exist" do
      shared.server
      |> User.delete(ID.new)
      |> should(eq :ok)

      "nosuchserver"
      |> User.delete(shared.id)
      |> should(eq :ok)
    end
  end

  describe "find/2" do
    context "when the user exists" do
      before do
        result = shared.id |> User.find(shared.server)
        {:ok, result: result}
      end

      it "returns a User struct" do
        shared.result |> should(be_struct User)
      end

      it "returns the right user" do
        shared.result.id |> should(eq shared.id)
        shared.result.server |> should(eq shared.server)
        shared.result.external_id |> should(eq shared.external_id)
      end
    end

    context "when the user does not exist" do
      it "returns nil" do
        ID.new
        |> User.find(shared.server)
        |> should(be_nil())

        shared.id
        |> User.find("nosuchserver")
        |> should(be_nil())
      end
    end
  end

  describe "search/2" do
    context "when there are matching users" do
      before do
        :ok = User.wait_for_user(shared.id)
        result = User.search(:external_id, shared.external_id)
        {:ok, result: result}
      end

      it "should return a list of User structs" do
        shared.result |> should(be_list())
        shared.result |> hd |> should(be_struct User)
      end

      it "should return matching users" do
        shared.result |> hd |> should(eq User.find(shared.id, shared.server))
      end
    end

    context "when there are no matching users" do
      before do
        result = User.search(:external_id, "nosuchuser")
        {:ok, result: result}
      end

      it "should return an empty list" do
        shared.result |> should(be_list())
        shared.result |> should(be_empty())
      end
    end
  end
end
