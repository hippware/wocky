defmodule Wocky.TROSFileSpec do
  use ESpec, async: true

  alias Faker.Lorem
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.TROSFile

  before do
    user = Factory.insert(:user)
    metadata = Factory.insert(:tros_metadata, %{user_id: user.id})
    {:ok,
     id: metadata.id,
     user_id: user.id,
     access: metadata.access}
  end

  describe "put/3" do
    context "when there is no existing metadata entry" do
      before do
        metadata = Factory.build(:tros_metadata, %{user_id: shared.user_id})
        result = TROSFile.put(metadata.id,
                              metadata.user_id,
                              metadata.access)
       {:ok,
        result: result,
        id: metadata.id,
        user_id: metadata.user_id,
        access: metadata.access}
      end

      it "should return :ok" do
        shared.result |> should(eq :ok)
      end

      it "should set the user_id and access values" do
        TROSFile.get_access(shared.id) |> should(eq shared.access)
        TROSFile.get_user_id(shared.id) |> should(eq shared.user_id)
      end
    end

    context "when there is an existing metadata entry" do
      before do
        metadata = Factory.build(:tros_metadata, %{user_id: shared.user_id})
        {:ok, metadata: metadata}
      end

      it "should return an error" do
        fn() -> TROSFile.put(shared.id,
                             shared.user_id,
                             shared.metadata.access)
        end
        |> should(raise_exception Ecto.ConstraintError)
      end
    end
  end

  describe "set_access/1" do
    context "when there is no existing entry for the file" do
      it "should raise an error" do
        fn() -> TROSFile.set_access(ID.new, Lorem.sentence()) end
        |> should(raise_exception Ecto.NoResultsError)
      end
    end

    context "when there is an existing entry for the file" do
      before do
        new_access = Lorem.sentence()
        result = TROSFile.set_access(shared.id, new_access)
        {:ok, access: new_access, result: result}
      end

      it "should return :ok" do
        shared.result |> should(eq :ok)
      end

      it "should update the access value" do
        TROSFile.get_access(shared.id) |> should(eq shared.access)
      end
    end
  end

  describe "get_user_id/2" do
    it "should get the user_id of an existing file" do
      TROSFile.get_user_id(shared.id) |> should(eq shared.user_id)
    end

    it "should return `nil` for a non-existant file" do
      TROSFile.get_user_id(ID.new) |> should(eq nil)
    end
  end

  describe "get_access/2" do
    it "should get the access data for an existing file" do
      TROSFile.get_access(shared.id) |> should(eq shared.access)
    end

    it "should return `nil` for a non-existant file" do
      TROSFile.get_access(ID.new) |> should(eq nil)
    end
  end
end
