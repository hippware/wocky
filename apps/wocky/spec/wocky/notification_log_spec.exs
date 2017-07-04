defmodule Wocky.NotificationLogSpec do
  use ESpec
  use Wocky.JID

  import Ecto.Query

  alias Faker.Lorem
  alias Wocky.NotificationLog
  alias Wocky.Repo
  alias Wocky.Repo.Factory

  describe "send/4" do
    before do
      user = Factory.insert(:user, %{server: shared.server})
      reference = make_ref()
      resource = Lorem.word
      result = NotificationLog.send(reference, user.id, resource, Lorem.sentence)

      {:ok,
        reference: reference,
        resource: resource,
        result: result,
        user: user
      }
    end

    it do: shared.result |> should(eq :ok)

    it "should insert a record" do
      results =
      NotificationLog
      |> where(user_id: ^shared.user.id)
      |> Repo.all

      results |> should(have_length 1)
      [result] = results
      result.reference |> :erlang.binary_to_term |> should(eq shared.reference)
    end

  end

  describe "result/3" do
    before do
      user = Factory.insert(:user, %{server: shared.server})
      reference = make_ref()
      reference_bin = :erlang.term_to_binary(reference)
      details = Lorem.sentence
      Factory.insert_list(2, :notification_log, user: user,
                          reference: reference_bin)
      result = NotificationLog.result(reference, true, details)

      {:ok,
        user: user,
        details: details,
        result: result
      }
    end

    it "should update the most recent entry with the given reference" do
      results =
      NotificationLog
      |> where(user_id: ^shared.user.id)
      |> order_by([desc: :created_at])
      |> Repo.all

      results |> should(have_length 2)
      [nl1, nl2] = results
      nl1.result |> should(be_true())
      nl1.details |> should(eq shared.details)
      nl2.result |> should(be_nil())
      nl2.details |> should(be_nil())
    end

    it "should return ok" do
      shared.result |> should(eq :ok)
    end
  end
end
