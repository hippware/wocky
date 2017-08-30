defmodule Wocky.PushNotifierSpec do
  use ESpec
  use Wocky.JID

  import Ecto.Query

  alias Faker.Code
  alias Pushex.Sandbox
  alias Wocky.PushNotifier
  alias Wocky.Repo
  alias Wocky.Repo.Factory

  @resource "testing"
  @platform "apple"
  @message  "Message content"

  before do
    user = Factory.insert(:user, %{server: shared.server})
    jid = JID.make(user.username, user.server, @resource)

    token = Code.isbn13
    result = PushNotifier.enable(jid, @platform, token)

    Sandbox.clear_notifications

    {:ok, user: user, jid: jid, token: token, result: result}
  end

  defp get_user_device(user, resource) do
    user
    |> Ecto.assoc(:devices)
    |> where([d], d.resource == ^resource)
    |> Repo.one
  end

  describe "enable/3" do
    it "should return :ok" do
      shared.result |> should(eq :ok)
    end

    it "should insert the token into the database" do
      row = get_user_device(shared.user, @resource)
      row.token |> should(eq shared.token)
    end
  end

  describe "disable/1" do
    before do
      result = PushNotifier.disable(shared.jid)
      {:ok, result: result}
    end

    it "should return :ok" do
      shared.result |> should(eq :ok)
    end

    it "should remove the database records" do
      get_user_device(shared.user, @resource) |> should(be_nil())
    end
  end

  describe "delete/1" do
    before do
      other_jid = jid(shared.jid, resource: "other", lresource: "other")
      _ = PushNotifier.enable(other_jid, @platform, "987654321")
      result = PushNotifier.delete(shared.jid)
      {:ok, result: result}
    end

    it "should return :ok" do
      shared.result |> should(eq :ok)
    end

    it "should remove all database records" do
      get_user_device(shared.user, @resource) |> should(be_nil())
    end
  end

  describe "push/2" do
    context "on success" do
      before do
        result = PushNotifier.push(shared.jid, @message)
        {:ok, result: result}
      end

      it "should return :ok" do
        shared.result |> should(eq :ok)
      end

      it "should send a push notification" do
        assert_receive {{:ok, _response}, _request, _ref}, 5000
      end
    end

    context "with a long message" do
      before do
        result = PushNotifier.push(shared.jid, Faker.Lorem.paragraph(100))
        {:ok, result: result}
      end

      it "should return :ok" do
        shared.result |> should(eq :ok)
      end

      it "should send a push notification" do
        assert_receive {{:ok, _response}, _request, _ref}, 5000
      end
    end

    context "on failure" do
      before do
        result = PushNotifier.push(shared.jid, "error")
        {:ok, result: result}
      end

      it "should return :ok" do
        shared.result |> should(eq :ok)
      end
    end
  end

  describe "push_all/2" do
    before do
      other_jid = jid(shared.jid, resource: "other", lresource: "other")
      _ = PushNotifier.enable(other_jid, @platform, "987654321")
      result = PushNotifier.push_all(shared.jid, @message)
      {:ok, result: result}
    end

    it "should return :ok" do
      shared.result |> should(eq :ok)
    end

    it "should send a push notification to each endpoint" do
      notifications = Sandbox.wait_notifications(count: 2, timeout: 5000)
      notifications |> Enum.count |> should(be :>=, 2)
    end
  end
end
