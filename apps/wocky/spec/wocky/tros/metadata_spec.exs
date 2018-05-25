# credo:disable-for-this-file Credo.Check.Refactor.PipeChainStart
defmodule Wocky.TROS.MetadataSpec do
  use ESpec, async: true

  alias Faker.Lorem
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.TROS.Metadata

  before do
    user = Factory.insert(:user)
    metadata = Factory.insert(:tros_metadata, user: user)
    {:ok, id: metadata.id, user: user, access: metadata.access, metadata: metadata}
  end

  describe "put/3" do
    context "when there is no existing metadata entry" do
      before do
        metadata = Factory.build(:tros_metadata, user: shared.user)
        result = Metadata.put(metadata.id, metadata.user.id, metadata.access)
        {:ok, result: result, id: metadata.id, access: metadata.access}
      end

      it "should return {:ok, TROSMetadata}" do
        should_be_result(shared.result)
      end

      it "should set the user_id and access values" do
        Metadata.get_access(shared.id) |> should(eq shared.access)
        Metadata.get_user_id(shared.id) |> should(eq shared.user.id)
      end
    end

    context "when there is an existing metadata entry" do
      before do
        metadata = Factory.build(:tros_metadata, user: shared.user)
        {:ok, metadata: metadata}
      end

      it "should return an error" do
        Metadata.put(shared.id, shared.user.id, shared.metadata.access)
        |> should(be_error_result())
      end
    end
  end

  describe "set_access/2" do
    context "when there is no existing entry for the file" do
      it "should return an error" do
        Metadata.set_access(ID.new(), Lorem.sentence())
        |> should(be_error_result())
      end
    end

    context "when there is an existing entry for the file" do
      before do
        new_access = Lorem.sentence()
        result = Metadata.set_access(shared.id, new_access)
        {:ok, access: new_access, result: result}
      end

      it "should return {:ok, Metadata}" do
        should_be_result(shared.result)
      end

      it "should update the access value" do
        Metadata.get_access(shared.id) |> should(eq shared.access)
      end
    end
  end

  describe "get_user_id/1" do
    it "should get the user_id of an existing file" do
      Metadata.get_user_id(shared.id) |> should(eq shared.user.id)
    end

    it "should return `nil` for a non-existant file" do
      Metadata.get_user_id(ID.new()) |> should(eq nil)
    end
  end

  describe "get_access/1" do
    it "should get the access data for an existing file" do
      Metadata.get_access(shared.id) |> should(eq shared.access)
    end

    it "should return `nil` for a non-existant file" do
      Metadata.get_access(ID.new()) |> should(eq nil)
    end
  end

  describe "ready?/1" do
    before do
      unready = Factory.insert(:tros_metadata, user: shared.user, ready: false)
      {:ok, unready: unready}
    end

    it do: assert(Metadata.ready?(shared.id))

    it do: refute(Metadata.ready?(shared.unready.id))
    it do: refute(Metadata.ready?(ID.new()))
  end

  defp should_be_result(result) do
    result |> should(be_ok_result())
    result |> elem(1) |> should(be_struct Metadata)
  end

  describe "delete/1" do
    context "when the data exists and is owned by the requestor" do
      before do
        result = Metadata.delete(shared.id, shared.user)
        {:ok, result: result}
      end

      it "should return {:ok, file}" do
        shared.result |> should(be_ok_result())
        shared.result |> elem(1) |> should(be_struct(Metadata))
        shared.result |> elem(1) |> Map.get(:id) |> should(eq shared.id)
      end

      it "should delete the file" do
        Metadata.get(shared.id) |> should(be_nil())
      end
    end

    context "when the data exists and is not owned by the requestor" do
      before do
        result = Metadata.delete(shared.id, Factory.insert(:user))
        {:ok, result: result}
      end

      it "should return {:error, :permission_denied}" do
        shared.result |> should(eq {:error, :permission_denied})
      end

      it "should delete the file" do
        Metadata.get(shared.id) |> should_not(be_nil())
      end
    end

    context "when the data does not exist" do
      before do
        result = Metadata.delete(ID.new(), shared.user)
        {:ok, result: result}
      end

      it "should return {:error, :not_found}" do
        shared.result |> should(eq {:error, :not_found})
      end
    end
  end
end
