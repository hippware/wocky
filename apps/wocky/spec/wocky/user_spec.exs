defmodule Wocky.UserSpec do
  use ESpec, async: true

  import Ecto.Query, only: [from: 2]

  alias Faker.Internet
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.Token
  alias Wocky.User

  before do
    user = Factory.insert(:user, %{server: shared.server})
    {:ok, user: user, id: user.id, external_id: user.external_id}
  end

  finally do
    Repo.delete_all(from u in User, where: u.id == ^shared.id)
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
    context "with an invalid ID" do

    end

    context "with a valid ID" do

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

    context "when passed a handle the same as the old" do
      before do
        result = User.update(shared.id, %{handle: shared.fields.handle})
        {:ok, result: result}
      end

      it "should return :ok" do
        shared.result |> should(eq :ok)
      end

      it "should not change the user's handle" do
        new_user = Repo.get(User, shared.id)
        new_user.handle |> should(eq shared.fields.handle)
      end
    end

    context "when passed a handle that already exists" do
      before do
        new_handle = Internet.user_name
        Factory.insert(:user, %{
                         server: shared.server,
                         handle: new_handle
                       })

        result = User.update(shared.id, %{handle: new_handle})
        {:ok, new_handle: new_handle, result: result}
      end

      it "should return a `duplicate_handle` error" do
        shared.result |> should(be_error_result())
        # shared.result |> should(eq {:error, :duplicate_handle})
      end

      it "should not update the user's attributes" do
        new_user = Repo.get(User, shared.id)
        new_user.handle |> should(eq shared.fields.handle)
      end
    end

    context "when passed a reserved handle" do
      before do
        result = User.update(shared.id, %{handle: "root"})
        {:ok, result: result}
      end

      it "should return a `duplicate_handle` error" do
        shared.result |> should(be_error_result())
        # shared.result |> should(eq {:error, :duplicate_handle})
      end

      it "should not update the user's attributes" do
        new_user = Repo.get(User, shared.id)
        new_user.handle |> should_not(eq "root")
      end
    end

    context "when passed an empty list of fields" do
      before do
        result = User.update(shared.id, %{})
        {:ok, result: result}
      end

      it "should return :ok" do
        shared.result |> should(eq :ok)
      end

      it "should not update the user's attributes" do
        new_user = Repo.get(User, shared.id)
        new_user.handle |> should(eq shared.fields.handle)
        new_user.first_name |> should(eq shared.fields.first_name)
        new_user.last_name |> should(eq shared.fields.last_name)
        new_user.email |> should(eq shared.fields.email)
      end
    end

    # TODO: Port avatar code to Wocky
  #   context "when a valid avatar is passed", async: false do
  #     before do
  #       server = shared.server
  #       avatar_id = ID.new
  #       avatar_url = :tros.make_url(shared.server, avatar_id)

  #       allow :tros |> to(accept get_metadata: fn ^server, ^avatar_id ->
  #                                  {:ok, %{owner: shared.id}}
  #                                end,
  #                                delete: fn _, _ ->
  #                                  :ok
  #                                end)

  #       {:ok, avatar_id: avatar_id, avatar_url: avatar_url}
  #     end

  #     context "and the user does not have an existing avatar" do
  #       before do
  #         result = User.update(shared.id, shared.server,
  #                              %{avatar: shared.avatar_url})

  #         {:ok, result: result}
  #       end

  #       it "should return :ok" do
  #         shared.result |> should(eq :ok)
  #       end

  #       it "should update the user's avatar" do
  #         new_user = User.find(shared.id, shared.server)
  #         new_user.avatar |> should(eq shared.avatar_url)
  #       end

  #       it "should not try to delete the old avatar" do
  #         :tros |> should_not(accepted :delete)
  #       end
  #     end

  #     context "and the user has an invalid avatar" do
  #       before do
  #         :ok = Repo.update(%{avatar: "bogus"}, "users",
  #                           shared.server, shared.id)

  #         result = User.update(shared.id, shared.server,
  #                              %{avatar: shared.avatar_url})

  #         {:ok, result: result}
  #       end

  #       it "should return :ok" do
  #         shared.result |> should(eq :ok)
  #       end

  #       it "should update the user's avatar" do
  #         new_user = User.find(shared.id, shared.server)
  #         new_user.avatar |> should(eq shared.avatar_url)
  #       end

  #       it "should not try to delete the old avatar" do
  #         :tros |> should_not(accepted :delete)
  #       end
  #     end

  #     context "and the user already has that avatar" do
  #       before do
  #         :ok = Repo.update(%{avatar: shared.avatar_url}, "users",
  #                           shared.server, shared.id)

  #         result = User.update(shared.id, shared.server,
  #                              %{avatar: shared.avatar_url})

  #         {:ok, result: result}
  #       end

  #       it "should return :ok" do
  #         shared.result |> should(eq :ok)
  #       end

  #       it "should not change the user's avatar" do
  #         new_user = User.find(shared.id, shared.server)
  #         new_user.avatar |> should(eq shared.avatar_url)
  #       end

  #       it "should not try to delete the avatar" do
  #         :tros |> should_not(accepted :delete)
  #       end
  #     end

  #     context "and the user has a valid avatar" do
  #       before do
  #         avatar_url = :tros.make_url(shared.server, ID.new)
  #         :ok = Repo.update(%{avatar: avatar_url}, "users",
  #                           shared.server, shared.id)

  #         result = User.update(shared.id, shared.server,
  #                              %{avatar: shared.avatar_url})

  #         {:ok, result: result}
  #       end

  #       it "should return :ok" do
  #         shared.result |> should(eq :ok)
  #       end

  #       it "should update the user's avatar" do
  #         new_user = User.find(shared.id, shared.server)
  #         new_user.avatar |> should(eq shared.avatar_url)
  #       end

  #       it "should delete the old avatar" do
  #         :tros |> should(accepted :delete)
  #       end
  #     end
  #   end

  #   context "when an avatar with a different owner is passed", async: false do
  #     before do
  #       server = shared.server
  #       avatar_id = ID.new

  #       allow :tros |> to(accept get_metadata: fn ^server, ^avatar_id ->
  #                                  {:ok, %{owner: ID.new}}
  #                                end)

  #       avatar_url = :tros.make_url(shared.server, avatar_id)
  #       result = User.update(shared.id, shared.server,
  #                            %{avatar: avatar_url})

  #       {:ok, result: result}
  #     end

  #     it "should return a `not_file_owner` error" do
  #       shared.result |> should(be_error_result())
  #       shared.result |> should(eq {:error, :not_file_owner})
  #     end

  #     it "should not update the user's avatar" do
  #       new_user = User.find(shared.id, shared.server)
  #       new_user.avatar |> should(be_nil())
  #     end
  #   end

  #   context "when a non-local avatar is passed" do
  #     before do
  #       avatar_url = :tros.make_url("nonlocal.com", ID.new)
  #       result = User.update(shared.id, shared.server,
  #                            %{avatar: avatar_url})

  #       {:ok, result: result}
  #     end

  #     it "should return a `not_local_file` error" do
  #       shared.result |> should(be_error_result())
  #       shared.result |> should(eq {:error, :not_local_file})
  #     end

  #     it "should not update the user's avatar" do
  #       new_user = User.find(shared.id, shared.server)
  #       new_user.avatar |> should(be_nil())
  #     end
  #   end

  #   context "when a non-existing avatar is passed", async: false do
  #     before do
  #       server = shared.server
  #       avatar_id = ID.new

  #       allow :tros |> to(accept get_metadata: fn ^server, ^avatar_id ->
  #                                  {:error, :not_found}
  #                                end)

  #       avatar_url = :tros.make_url(shared.server, avatar_id)
  #       result = User.update(shared.id, shared.server,
  #                            %{avatar: avatar_url})

  #       {:ok, result: result}
  #     end

  #     it "should return a `not_found` error" do
  #       shared.result |> should(be_error_result())
  #       shared.result |> should(eq {:error, :not_found})
  #     end

  #     it "should not update the user's avatar" do
  #       new_user = User.find(shared.id, shared.server)
  #       new_user.avatar |> should(be_nil())
  #     end
  #   end
  end

  describe "set_avatar/2" do

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

  end

  describe "to_jid_string/1" do

  end

  describe "to_bare_jid/1" do

  end

  describe "to_bare_jid_string/1" do

  end
end
