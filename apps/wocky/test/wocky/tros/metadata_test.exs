defmodule Wocky.TROS.MetadataTest do
  use Wocky.DataCase, async: true

  alias Faker.Lorem
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.TROS.Metadata

  setup do
    user = Factory.insert(:user)
    metadata = Factory.insert(:tros_metadata, user: user)

    {:ok,
     id: metadata.id, user: user, access: metadata.access, metadata: metadata}
  end

  describe "put/3 when there is no existing metadata entry" do
    setup ctx do
      metadata = Factory.build(:tros_metadata, user: ctx.user)
      result = Metadata.put(metadata.id, metadata.user.id, metadata.access)
      {:ok, result: result, id: metadata.id, access: metadata.access}
    end

    test "should return {:ok, TROSMetadata}", ctx do
      assert {:ok, %Metadata{}} = ctx.result
    end

    test "should set the user_id and access values", ctx do
      metadata = Metadata.get(ctx.id)
      assert metadata.access == ctx.access
      assert metadata.user_id == ctx.user.id
    end
  end

  describe "put/3 when there is an existing metadata entry" do
    setup ctx do
      metadata = Factory.build(:tros_metadata, user: ctx.user)
      {:ok, metadata: metadata, access: metadata.access}
    end

    test "should return an error", ctx do
      assert {:error, _} = Metadata.put(ctx.id, ctx.user.id, ctx.access)
    end
  end

  describe "set_access/2 when there is no existing entry for the file" do
    test "should return an error" do
      assert {:error, _} = Metadata.set_access(ID.new(), Lorem.sentence())
    end
  end

  describe "set_access/2 when there is an existing entry for the file" do
    setup ctx do
      new_access = Lorem.sentence()
      result = Metadata.set_access(ctx.id, new_access)
      {:ok, access: new_access, result: result}
    end

    test "should return {:ok, Metadata}", ctx do
      assert {:ok, %Metadata{}} = ctx.result
    end

    test "should update the access value", ctx do
      assert Metadata.get(ctx.id).access == ctx.access
    end
  end

  test "ready?/1", ctx do
    unready = Factory.insert(:tros_metadata, user: ctx.user, ready: false)

    assert Metadata.ready?(ctx.id)

    refute Metadata.ready?(unready.id)
    refute Metadata.ready?(ID.new())
  end

  describe "delete/1 when the data exists and is owned by the requestor" do
    setup ctx do
      result = Metadata.delete(ctx.id, ctx.user)
      {:ok, result: result}
    end

    test "should return {:ok, file}", ctx do
      assert {:ok, %Metadata{} = metadata} = ctx.result
      assert metadata.id == ctx.id
    end

    test "should delete the file", ctx do
      refute Metadata.get(ctx.id)
    end
  end

  describe "delete/1 when the data exists and is not owned by the requestor" do
    setup ctx do
      result = Metadata.delete(ctx.id, Factory.insert(:user))
      {:ok, result: result}
    end

    test "should return {:error, :permission_denied}", ctx do
      assert ctx.result == {:error, :permission_denied}
    end

    test "should not delete the file", ctx do
      assert Metadata.get(ctx.id)
    end
  end

  describe "delete/1 when the data does not exist" do
    setup ctx do
      result = Metadata.delete(ID.new(), ctx.user)
      {:ok, result: result}
    end

    test "should return {:error, :not_found}", ctx do
      assert ctx.result == {:error, :not_found}
    end
  end
end
