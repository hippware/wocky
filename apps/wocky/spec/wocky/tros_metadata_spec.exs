defmodule Wocky.TROSMetadataSpec do
  use ESpec, async: true

  alias Faker.Lorem
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.TROSMetadata

  before do
    user = Factory.insert(:user)
    metadata = Factory.insert(:tros_metadata, user: user)
    {:ok,
     id: metadata.id,
     user: user,
     access: metadata.access}
  end

  describe "put/3" do
    context "when there is no existing metadata entry" do
      before do
        metadata = Factory.build(:tros_metadata, user: shared.user)
        result = TROSMetadata.put(metadata.id,
                                  metadata.user.id,
                                  metadata.access)
        {:ok,
         result: result,
         id: metadata.id,
         access: metadata.access}
      end

      it "should return {:ok, TROSMetadata}" do
        should_be_result(shared.result)
      end

      it "should set the user_id and access values" do
        TROSMetadata.get_access(shared.id) |> should(eq shared.access)
        TROSMetadata.get_user_id(shared.id) |> should(eq shared.user.id)
      end
    end

    context "when there is an existing metadata entry" do
      before do
        metadata = Factory.build(:tros_metadata, user: shared.user)
        {:ok, metadata: metadata}
      end

      it "should return an error" do
        TROSMetadata.put(shared.id, shared.user.id, shared.metadata.access)
        |> should(be_error_result())
      end
    end
  end

  describe "set_access/2" do
    context "when there is no existing entry for the file" do
      it "should return an error" do
        TROSMetadata.set_access(ID.new, Lorem.sentence())
        |> should(be_error_result())
      end
    end

    context "when there is an existing entry for the file" do
      before do
        new_access = Lorem.sentence()
        result = TROSMetadata.set_access(shared.id, new_access)
        {:ok, access: new_access, result: result}
      end

      it "should return {:ok, TROSMetadata}" do
        should_be_result(shared.result)
      end

      it "should update the access value" do
        TROSMetadata.get_access(shared.id) |> should(eq shared.access)
      end
    end
  end

  describe "get_user_id/1" do
    it "should get the user_id of an existing file" do
      TROSMetadata.get_user_id(shared.id) |> should(eq shared.user.id)
    end

    it "should return `nil` for a non-existant file" do
      TROSMetadata.get_user_id(ID.new) |> should(eq nil)
    end
  end

  describe "get_access/1" do
    it "should get the access data for an existing file" do
      TROSMetadata.get_access(shared.id) |> should(eq shared.access)
    end

    it "should return `nil` for a non-existant file" do
      TROSMetadata.get_access(ID.new) |> should(eq nil)
    end
  end

  defp should_be_result(result) do
    result |> should(be_ok_result())
    result |> elem(1) |> should(be_struct TROSMetadata)
  end
end
