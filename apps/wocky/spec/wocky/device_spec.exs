defmodule Wocky.DeviceSpec do
  use ESpec, async: true
  use ModelHelpers

  alias Wocky.Device
  alias Wocky.Repo.ID

  before do
    user = Factory.insert(:user, %{server: shared.server})
    resource = Faker.Code.issn
    device = Faker.Code.issn
    endpoint = Faker.Code.issn

    result = Device.update(user.id, resource, :apple, device, endpoint)

    {:ok,
      result: result,
      id: user.id,
      resource: resource,
      device: device,
      endpoint: endpoint}
  end

  describe "update/5" do
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
      data.device     |> should(eq shared.device)
      data.endpoint   |> should(eq shared.endpoint)
    end

    it "should overwrite devices when there are multiple requests" do
      :ok = Device.update(shared.id, shared.resource,
                          :apple, Faker.Code.issn, Faker.Code.issn)

      data = Repo.one(
        from d in Device,
        where: d.user_id == ^shared.id,
        where: d.resource == ^shared.resource
      )

      data.user_id    |> should(eq shared.id)
      data.resource   |> should(eq shared.resource)
      data.device     |> should_not(eq shared.device)
      data.endpoint   |> should_not(eq shared.endpoint)
    end
  end

  describe "get_endpoint/2" do
    it "should return the device assigned to a resource" do
      shared.id
      |> Device.get_endpoint(shared.resource)
      |> should(eq shared.endpoint)
    end

    it "should return nil if the resource is not assigned a device" do
      ID.new
      |> Device.get_endpoint(shared.resource)
      |> should(be_nil())

      shared.id
      |> Device.get_endpoint("nosuchresource")
      |> should(be_nil())
    end
  end

  describe "get_all_endpoints/1" do
    it "should return a list of endpoints for the specified user" do
      shared.id
      |> Device.get_all_endpoints
      |> should(eq [shared.endpoint])
    end

    it "should return an empty list if the user has no assigned devices" do
      ID.new
      |> Device.get_all_endpoints
      |> should(be_empty())
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
