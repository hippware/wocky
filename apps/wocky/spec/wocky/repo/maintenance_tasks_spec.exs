defmodule Wocky.Repo.MaintenanceTasksSpec do
  use ESpec, async: true

  import Ecto.Query
  import SweetXml

  alias Wocky.Bot
  alias Wocky.Bot.Item
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.Repo.MaintenanceTasks
  alias Wocky.Repo.Timestamp
  alias Wocky.Token
  alias Wocky.TrafficLog
  alias Wocky.TROS
  alias Wocky.TROS.Metadata
  alias Wocky.User

  before do
    user = Factory.insert(:user)
    {:ok, user: user}
  end

  describe "clean_pending_bots" do
    before do
      # Pending Bot, recent create date
      new_pending = Factory.insert(:bot, user: shared.user, pending: true)

      # Pending Bot, older create date
      old_pending = Factory.insert(:bot, user: shared.user, pending: true,
                                   created_at: Timestamp.shift(days: -2))

      # Non-pending bot, recent create date
      new_nonpending = Factory.insert(:bot, user: shared.user, pending: false)

      # Non-pending bot, older create date
      old_nonpending = Factory.insert(:bot, user: shared.user, pending: false,
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
      new = Factory.insert(:traffic_log, user: shared.user)
      old = Factory.insert(:traffic_log, user: shared.user,
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
      {:ok, {_, _}} = Token.assign(shared.user.id, "old_token")
      {:ok, {_, _}} = Token.assign(shared.user.id, "new_token")

      query = from t in Token,
                where: t.user_id == ^shared.user.id,
                where: t.resource == ^"old_token"

      query
      |> Repo.one
      |> Token.changeset(%{expires_at: Timestamp.shift(weeks: -1)})
      |> Repo.update!

      {:ok, result} = MaintenanceTasks.clean_expired_auth_tokens
      {:ok, result: result}
    end

    it "should return the number of tokens removed" do
      shared.result |> should(eq 1)
    end

    it "should remove the old token" do
      query = from t in Token,
                where: t.user_id == ^shared.user.id,
                where: t.resource == ^"old_token"

      query
      |> Repo.one
      |> should(be_nil())
    end

    it "should not remove the recent token" do
      query = from t in Token,
                where: t.user_id == ^shared.user.id,
                where: t.resource == ^"new_token"

      query
      |> Repo.one
      |> should_not(be_nil())
    end
  end

  describe "clean_pending_tros_files" do
    before do
      # Pending file, recent create date
      new_pending = Factory.insert(:tros_metadata,
                                   user: shared.user, ready: false)

      # Pending file, older create date
      old_pending = Factory.insert(:tros_metadata,
                                   user: shared.user, ready: false,
                                   created_at: Timestamp.shift(weeks: -2))

      # Non-pending file, recent create date
      new_nonpending = Factory.insert(:tros_metadata,
                                      user: shared.user, ready: true)

      # Non-pending file, older create date
      old_nonpending = Factory.insert(:tros_metadata,
                                      user: shared.user, ready: true,
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

  defp item_stanza(opts) do
    """
    <entry xmlns='http://www.w3.org/2005/Atom'>
      <title/>
      #{content_tag(opts[:content])}
      #{image_tag(opts[:image])}
    </entry>
    """
  end

  defp image_tag(nil), do: ""
  defp image_tag(image), do: "<image>#{image}</image>"

  defp content_tag(nil), do: ""
  defp content_tag(text), do: "<content>#{text}</content>"

  describe "clean_dead_tros_links" do
    before do
      md = Factory.insert(:tros_metadata, user: shared.user)
      {:ok, md: md, file_url: TROS.make_url("localhost", md.id)}
    end

    describe "clean_bot_item_image_links" do
      before do
        bad_url = TROS.make_url("localhost", ID.new)
        bot = Factory.insert(:bot, user: shared.user)

        good_with_content =
          Factory.insert(:item, bot: bot, user: shared.user, image: true,
                         stanza: item_stanza(image: shared.file_url,
                                             content: "testing"))
        bad_with_content =
          Factory.insert(:item, bot: bot, user: shared.user, image: true,
                         stanza: item_stanza(image: bad_url,
                                             content: "testing"))
        good_no_content =
          Factory.insert(:item, bot: bot, user: shared.user, image: true,
                         stanza: item_stanza(image: shared.file_url))
        bad_no_content =
          Factory.insert(:item, bot: bot, user: shared.user, image: true,
                         stanza: item_stanza(image: bad_url))
        only_content =
          Factory.insert(:item, bot: bot, user: shared.user, image: false,
                         stanza: item_stanza(content: "testing"))

        {:ok, result} = MaintenanceTasks.clean_bot_item_image_links(true)
        {:ok, [
          bot: bot,
          good_with_content: good_with_content,
          bad_with_content: bad_with_content,
          good_no_content: good_no_content,
          bad_no_content: bad_no_content,
          only_content: only_content,
          result: result
        ]}
      end

      it "should return the number of images purged" do
        shared.result |> should(eq 2)
      end

      context "when the item stanza has content" do
        it "should not delete an item with an invalid image link" do
          Item
          |> where(id: ^shared.bad_with_content.id)
          |> where(bot_id: ^shared.bot.id)
          |> Repo.one
          |> should_not(be_nil())
        end

        it "should remove the invalid image link from the item" do
          Item
          |> where(id: ^shared.bad_with_content.id)
          |> where(bot_id: ^shared.bot.id)
          |> select([i], i.stanza)
          |> Repo.one
          |> xpath(~x"/entry/image/text()"S)
          |> should(eq "")
        end

        it "should not change a stanza with valid image link" do
          Item
          |> where(id: ^shared.good_with_content.id)
          |> where(bot_id: ^shared.bot.id)
          |> select([i], i.stanza)
          |> Repo.one
          |> should(eq shared.good_with_content.stanza)
        end

        it "should not change a stanza with no image link" do
          Item
          |> where(id: ^shared.only_content.id)
          |> where(bot_id: ^shared.bot.id)
          |> select([i], i.stanza)
          |> Repo.one
          |> should(eq shared.only_content.stanza)
        end
      end

      context "when the item stanza has no content" do
        it "should delete an item with an invalid image link" do
          Item
          |> where(id: ^shared.bad_no_content.id)
          |> where(bot_id: ^shared.bot.id)
          |> Repo.one
          |> should(be_nil())
        end

        it "should not delete an item with a valid image link" do
          Item
          |> where(id: ^shared.good_no_content.id)
          |> where(bot_id: ^shared.bot.id)
          |> Repo.one
          |> should_not(be_nil())
        end
      end
    end

    describe "clean_bot_image_links" do
      before do
        invalid_bot = Factory.insert(:bot, user: shared.user)
        valid_bot = Factory.insert(:bot, user: shared.user,
                                   image: shared.file_url)

        {:ok, result} = MaintenanceTasks.clean_bot_image_links(true)
        {:ok, result: result, invalid_bot: invalid_bot, valid_bot: valid_bot}
      end

      it "should return the number of images nillified" do
        shared.result |> should(eq 1)
      end

      it "should nillify non-existing images" do
        Bot
        |> where(id: ^shared.invalid_bot.id)
        |> select([b], b.image)
        |> Repo.one
        |> should(be_nil())
      end

      it "should leave existing images" do
        Bot
        |> where(id: ^shared.valid_bot.id)
        |> select([b], b.image)
        |> Repo.one
        |> should(eq shared.file_url)
      end
    end

    describe "clean_user_avatar_links" do
      before do
        valid_user = Factory.insert(:user, avatar: shared.file_url)
        {:ok, result} = MaintenanceTasks.clean_user_avatar_links(true)
        {:ok, result: result, valid_user: valid_user}
      end

      it "should return the number of avatars nillified" do
        shared.result |> should(eq 1)
      end

      it "should nillify non-existing avatars" do
        User
        |> where(id: ^shared.user.id)
        |> select([u], u.avatar)
        |> Repo.one
        |> should(be_nil())
      end

      it "should leave existing avatars" do
        User
        |> where(id: ^shared.valid_user.id)
        |> select([u], u.avatar)
        |> Repo.one
        |> should(eq shared.file_url)
      end
    end
  end
end
