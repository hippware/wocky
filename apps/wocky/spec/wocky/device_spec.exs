defmodule Wocky.DeviceSpec do
  use ESpec, async: true
  use ModelHelpers

  alias Wocky.Device
  alias Wocky.Repo.ID

  before do
    user = Factory.insert(:user, %{server: shared.server})
    resource = Faker.Code.issn
    token = Faker.Code.issn

    result = Device.update(user.id, resource, :apple, token)

    {:ok,
      result: result,
      id: user.id,
      resource: resource,
      token: token,
      user: user
    }
  end

  describe "update/4" do
    it "should return :ok" do
      shared.result |> should(eq :ok)
    end

    it "should store the device for the user" do
      data = Repo.one(
        from d in Device,
        where: d.user_id == ^shared.id,
        where: d.resource == ^shared.resource
      )

      data.user_id    |> should(eq shared.id)
      data.resource   |> should(eq shared.resource)
      data.platform   |> should(eq "apple")
      data.token      |> should(eq shared.token)
    end

    it "should overwrite devices when there are multiple requests" do
      :ok = Device.update(shared.id, shared.resource,
                          :apple, Faker.Code.issn)

      data = Repo.one(
        from d in Device,
        where: d.user_id == ^shared.id,
        where: d.resource == ^shared.resource
      )

      data.user_id    |> should(eq shared.id)
      data.resource   |> should(eq shared.resource)
      data.token      |> should_not(eq shared.token)
    end
  end

  describe "get/1" do
    it "should return all valid devices for a user" do
      devices = Device.get(shared.user.id)
      devices |> should(have_length 1)
      [device] = devices
      device.user_id |> should(eq shared.user.id)
      device.token |> should(eq shared.token)
      device.resource |> should(eq shared.resource)
    end

    it "should return an empty list if no valid devices exist for the user" do
      Device.get(ID.new) |> should(eq [])
    end
  end

  describe "get_by_token/1" do
    it "should return the device for a token" do
      device = Device.get_by_token(shared.token)
      device.user_id |> should(eq shared.user.id)
      device.resource |> should(eq shared.resource)
      device.token |> should(eq shared.token)
    end

    it "should return nil if no device is associated with the token" do
      "foobar" |> Device.get_by_token |> should(be_nil())
    end
  end

  describe "get_token/2" do
    it "should return the valid token assigned to a resource" do
      shared.id
      |> Device.get_token(shared.resource)
      |> should(eq shared.token)
    end

    it "should return nil if the resource is not assigned a valid token" do
      ID.new
      |> Device.get_token(shared.resource)
      |> should(be_nil())

      shared.id
      |> Device.get_token("nosuchresource")
      |> should(be_nil())
    end
  end

  describe "get_all_tokens/1" do
    it "should return a list of valid tokens for the specified user" do
      shared.id
      |> Device.get_all_tokens
      |> should(eq [shared.token])
    end

    it "should return an empty list if the user has no valid tokens" do
      ID.new
      |> Device.get_all_tokens
      |> should(be_empty())
    end
  end

  describe "invalidate/2" do
    context "when feedback is false" do
      before do
        result = Device.invalidate(shared.token)
        {:ok, result: result}
      end

      it "should return :ok" do
        shared.result |> should(eq :ok)
      end

      it "should set invalid to true" do
        device =
          Device
          |> where(token: ^shared.token)
          |> Repo.one

        device.invalid |> should(eq true)
      end

      it "should set feedback to false" do
        device =
          Device
          |> where(token: ^shared.token)
          |> Repo.one

        device.feedback |> should(eq false)
      end
    end

    context "when feedback is true" do
      before do
        result = Device.invalidate(shared.token, true)
        {:ok, result: result}
      end

      it "should return :ok" do
        shared.result |> should(eq :ok)
      end

      it "should set invalid to true" do
        device =
          Device
          |> where(token: ^shared.token)
          |> Repo.one

        device.invalid |> should(eq true)
      end

      it "should set feedback to true" do
        device =
          Device
          |> where(token: ^shared.token)
          |> Repo.one

        device.feedback |> should(eq true)
      end
    end
  end

  describe "delete/2" do
    before do
      result = Device.delete(shared.id, shared.resource)
      {:ok, result: result}
    end

    it "should return :ok" do
      shared.result |> should(eq :ok)
    end

    it "should remove the device from the database" do
      query = from d in Device,
                where: d.user_id == ^shared.id,
                where: d.resource == ^shared.resource

      query
      |> Repo.one
      |> should(be_nil())
    end

    it "should return :ok if the device has already been removed" do
      shared.id
      |> Device.delete(shared.resource)
      |> should(eq :ok)
    end

    it "should return :ok if there never was a device" do
      shared.id
      |> Device.delete("nosuchresource")
      |> should(eq :ok)
    end
  end

  describe "delete_all/1" do
    before do
      result = Device.delete_all(shared.id)
      {:ok, result: result}
    end

    it "should return :ok" do
      shared.result |> should(eq :ok)
    end

    it "should remove all devices from the database" do
      query = from d in Device, where: d.user_id == ^shared.id

      query
      |> Repo.all
      |> should(be_empty())
    end

    it "should return :ok if the user doesn't have any devices" do
      ID.new
      |> Device.delete_all
      |> should(eq :ok)
    end
  end
end
