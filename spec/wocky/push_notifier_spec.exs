defmodule Wocky.PushNotifierSpec do
  use ESpec
  use Wocky.JID

  alias Wocky.PushNotifier
  alias Wocky.PushNotifier.TestBackend
  alias :wocky_db, as: WockyDb

  @user          "043e8c96-ba30-11e5-9912-ba0be0483c18"
  @server        "localhost"
  @resource      "testing"
  @platform      "apple"
  @local_context "localhost"
  @user_jid      JID.make(@user, @server, @resource)
  @device_id     "123456789"
  @message       "Message content"

  before do
    WockyDb.clear_tables(@local_context, [:device])
    TestBackend.reset

    result = PushNotifier.enable(@user_jid, @platform, @device_id)
    {:ok, result: result}
  end

  describe "enable/3" do
    context "on success" do
      it "should return :ok" do
        shared.result |> should(be_ok_result())
      end

      it "should register the device with the backend" do
        [{_, jid, platform, device_id}] = TestBackend.get_registrations
        jid |> should(eq JID.to_binary(@user_jid))
        platform |> should(eq @platform)
        device_id |> should(eq @device_id)
      end

      it "should insert the device_id and endpoint into the database" do
        row = WockyDb.select_row(@local_context, :device, :all,
          %{user: @user, server: @server, resource: @resource})

        {:ok, endpoint} = shared.result

        row.device_id |> should(eq @device_id)
        row.endpoint |> should(eq endpoint)
      end
    end

    context "on failure" do
      before do
        WockyDb.clear_tables(@local_context, [:device])
        TestBackend.reset

        result = PushNotifier.enable(@user_jid, @platform, "error")
        {:ok, result: result}
      end

      it "should return an error" do
        shared.result |> should(be_error_result())
      end

      it "should not register the device" do
        TestBackend.get_registrations |> should(eq [])
      end

      it "should not insert anything into the database" do
        row = WockyDb.select_row(@local_context, :device, :all,
          %{user: @user, server: @server, resource: @resource})

        row |> should(eq :not_found)
      end
    end
  end

  describe "disable/1" do
    before do
      result = PushNotifier.disable(@user_jid)
      {:ok, result: result}
    end

    it "should return :ok" do
      shared.result |> should(eq :ok)
    end

    it "should remove the device registration" do
      TestBackend.get_registrations |> should(eq [])
    end

    it "should remove the database records" do
      row = WockyDb.select_row(@local_context, :device, :all,
        %{user: @user, server: @server, resource: @resource})

      row |> should(eq :not_found)
    end
  end

  describe "delete/1" do
    before do
      other_jid = JID.make(@user, @server, "other")
      _ = PushNotifier.enable(other_jid, @platform, "987654321")
      result = PushNotifier.delete(@user_jid)
      {:ok, result: result}
    end

    it "should return :ok" do
      shared.result |> should(eq :ok)
    end

    it "should remove all device registrations" do
      TestBackend.get_registrations |> should(eq [])
    end

    it "should remove all database records" do
      row = WockyDb.select_row(@local_context, :device, :all,
        %{user: @user, server: @server, resource: @resource})

      row |> should(eq :not_found)
    end
  end

  describe "push/2" do
    before do
      result = PushNotifier.push(@user_jid, @message)
      {:ok, result: result}
    end

    it "should return :ok" do
      shared.result |> should(eq :ok)
    end

    it "should send a push notification" do
      [{_, message}] = TestBackend.get_notifications
      message |> should(eq @message)
    end
  end

  describe "push_all/2" do
    before do
      other_jid = JID.make(@user, @server, "other")
      _ = PushNotifier.enable(other_jid, @platform, "987654321")
      result = PushNotifier.push_all(@user_jid, @message)
      {:ok, result: result}
    end

    it "should return :ok" do
      shared.result |> should(eq :ok)
    end

    it "should send a push notification to each endpoint" do
      TestBackend.get_notifications |> should(have_size 2)
    end
  end
end
