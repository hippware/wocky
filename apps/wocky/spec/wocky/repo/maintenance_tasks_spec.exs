defmodule Wocky.Repo.MaintenanceTasksSpec do
  use ESpec, async: true

  import Ecto.Query

  alias Wocky.Bot
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Repo.MaintenanceTasks
  alias Wocky.Repo.Timestamp
  alias Wocky.Token
  alias Wocky.TrafficLog
  alias Wocky.TROS.Metadata


  describe "clean_pending_bots" do
    before do
      # Pending Bot, recent create date
      new_pending = Factory.insert(:bot, pending: true)

      # Pending Bot, older create date
      old_pending = Factory.insert(:bot, pending: true,
                                   created_at: Timestamp.shift(days: -2))

      # Non-pending bot, recent create date
      new_nonpending = Factory.insert(:bot, pending: false)

      # Non-pending bot, older create date
      old_nonpending = Factory.insert(:bot, pending: false,
                                      created_at: Timestamp.shift(days: -2))

      {:ok, result} = MaintenanceTasks.clean_pending_bots
      {:ok, [
        new_pending: new_pending,
        old_pending: old_pending,
        new_nonpending: new_nonpending,
        old_nonpending: old_nonpending,
        result: result
      ]}
    end

    it "should return the number of bots removed" do
      shared.result |> should(eq 1)
    end

    it "should remove the old, pending bot" do
      Bot
      |> Repo.get(shared.old_pending.id)
      |> should(be_nil())
    end

    it "should not remove the recent, pending bot" do
      Bot
      |> Repo.get(shared.new_pending.id)
      |> should_not(be_nil())
    end

    it "should not remove the old, non-pending bot" do
      Bot
      |> Repo.get(shared.old_nonpending.id)
      |> should_not(be_nil())
    end

    it "should not remove the recent, non-pending bot" do
      Bot
      |> Repo.get(shared.new_nonpending.id)
      |> should_not(be_nil())
    end
  end

  describe "clean_traffic_logs" do
    before do
      new = Factory.insert(:traffic_log)
      old = Factory.insert(:traffic_log,
                           created_at: Timestamp.shift(months: -2))

      {:ok, result} = MaintenanceTasks.clean_traffic_logs
      {:ok, result: result, new: new, old: old}
    end

    it "should return the number of log entries removed" do
      shared.result |> should(eq 1)
    end

    it "should remove the old log entry" do
      TrafficLog
      |> Repo.get(shared.old.id)
      |> should(be_nil())
    end

    it "should not remove the recent log entry" do
      TrafficLog
      |> Repo.get(shared.new.id)
      |> should_not(be_nil())
    end
  end

  describe "clean_expired_auth_tokens" do
    before do
      user = Factory.insert(:user)

      {:ok, {_, _}} = Token.assign(user.id, "old_token")
      {:ok, {_, _}} = Token.assign(user.id, "new_token")

      query = from t in Token,
                where: t.user_id == ^user.id,
                where: t.resource == ^"old_token"

      query
      |> Repo.one
      |> Token.changeset(%{expires_at: Timestamp.shift(weeks: -1)})
      |> Repo.update!

      {:ok, result} = MaintenanceTasks.clean_expired_auth_tokens
      {:ok, result: result, user_id: user.id}
    end

    it "should return the number of tokens removed" do
      shared.result |> should(eq 1)
    end

    it "should remove the old token" do
      query = from t in Token,
                where: t.user_id == ^shared.user_id,
                where: t.resource == ^"old_token"

      query
      |> Repo.one
      |> should(be_nil())
    end

    it "should not remove the recent token" do
      query = from t in Token,
                where: t.user_id == ^shared.user_id,
                where: t.resource == ^"new_token"

      query
      |> Repo.one
      |> should_not(be_nil())
    end
  end

  describe "clean_pending_tros_files" do
    before do
      # Pending file, recent create date
      new_pending = Factory.insert(:tros_metadata, ready: false)

      # Pending file, older create date
      old_pending = Factory.insert(:tros_metadata, ready: false,
                                   created_at: Timestamp.shift(weeks: -2))

      # Non-pending file, recent create date
      new_nonpending = Factory.insert(:tros_metadata, ready: true)

      # Non-pending file, older create date
      old_nonpending = Factory.insert(:tros_metadata, ready: true,
                                      created_at: Timestamp.shift(weeks: -2))

      {:ok, result} = MaintenanceTasks.clean_pending_tros_files
      {:ok, [
        new_pending: new_pending,
        old_pending: old_pending,
        new_nonpending: new_nonpending,
        old_nonpending: old_nonpending,
        result: result
      ]}
    end

    it "should return the number of metadatas removed" do
      shared.result |> should(eq 1)
    end

    it "should remove the old, pending metadata" do
      Metadata
      |> Repo.get(shared.old_pending.id)
      |> should(be_nil())
    end

    it "should not remove the recent, pending metadata" do
      Metadata
      |> Repo.get(shared.new_pending.id)
      |> should_not(be_nil())
    end

    it "should not remove the old, non-pending metadata" do
      Metadata
      |> Repo.get(shared.old_nonpending.id)
      |> should_not(be_nil())
    end

    it "should not remove the recent, non-pending metadata" do
      Metadata
      |> Repo.get(shared.new_nonpending.id)
      |> should_not(be_nil())
    end
  end

  describe "clean_dead_tros_links" do

    describe "clean_bot_item_image_links" do

    end

    describe "clean_bot_image_links" do

    end

    describe "clean_user_avatar_links" do

    end
  end
end
