defmodule Wocky.Repo.CleanerTest do
  use Wocky.DataCase

  import Ecto.Query
  import SweetXml

  alias Wocky.Account
  alias Wocky.Account.Token, as: AuthToken
  alias Wocky.Bot
  alias Wocky.Bot.Item
  alias Wocky.Push.Log, as: PushLog
  alias Wocky.Push.Token, as: PushToken
  alias Wocky.Repo
  alias Wocky.Repo.Cleaner
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.Repo.Timestamp
  alias Wocky.TrafficLog
  alias Wocky.TROS
  alias Wocky.TROS.Metadata
  alias Wocky.User
  alias Wocky.User.InviteCode

  setup do
    user = Factory.insert(:user)
    {:ok, user: user}
  end

  describe "clean_pending_bots" do
    setup %{user: user} do
      # Pending Bot, recent create date
      new_pending = Factory.insert(:bot, user: user, pending: true)

      # Pending Bot, older create date
      old_pending =
        Factory.insert(
          :bot,
          user: user,
          pending: true,
          created_at: Timestamp.shift(days: -2)
        )

      # Non-pending bot, recent create date
      new_nonpending = Factory.insert(:bot, user: user, pending: false)

      # Non-pending bot, older create date
      old_nonpending =
        Factory.insert(
          :bot,
          user: user,
          pending: false,
          created_at: Timestamp.shift(days: -2)
        )

      {:ok, result} = Cleaner.clean_pending_bots()

      {:ok,
       new_pending: new_pending,
       old_pending: old_pending,
       new_nonpending: new_nonpending,
       old_nonpending: old_nonpending,
       result: result}
    end

    test "should return the number of bots removed", %{result: result} do
      assert result == 1
    end

    test "should remove the old, pending bot", ctx do
      refute Repo.get(Bot, ctx.old_pending.id)
    end

    test "should not remove the recent, pending bot", ctx do
      assert Repo.get(Bot, ctx.new_pending.id)
    end

    test "should not remove the old, non-pending bot", ctx do
      assert Repo.get(Bot, ctx.old_nonpending.id)
    end

    test "should not remove the recent, non-pending bot", ctx do
      assert Repo.get(Bot, ctx.new_nonpending.id)
    end
  end

  describe "clean_traffic_logs" do
    setup %{user: user} do
      new = Factory.insert(:traffic_log, user: user)

      old =
        Factory.insert(
          :traffic_log,
          user: user,
          created_at: Timestamp.shift(months: -2)
        )

      {:ok, result} = Cleaner.clean_traffic_logs()
      {:ok, result: result, new: new, old: old}
    end

    test "should return the number of log entries removed", %{result: result} do
      assert result == 1
    end

    test "should remove the old log entry", %{old: old} do
      refute Repo.get(TrafficLog, old.id)
    end

    test "should not remove the recent log entry", %{new: new} do
      assert Repo.get(TrafficLog, new.id)
    end
  end

  describe "clean_expired_auth_tokens" do
    setup %{user: user} do
      {:ok, {_, _}} = Account.assign_token(user.id, "old_token")
      {:ok, {_, _}} = Account.assign_token(user.id, "new_token")

      query =
        from t in AuthToken,
          where: t.user_id == ^user.id,
          where: t.resource == ^"old_token"

      query
      |> Repo.one()
      |> AuthToken.changeset(%{expires_at: Timestamp.shift(weeks: -1)})
      |> Repo.update!()

      {:ok, result} = Cleaner.clean_expired_auth_tokens()
      {:ok, result: result}
    end

    test "should return the number of tokens removed", %{result: result} do
      assert result == 1
    end

    test "should remove the old token", %{user: user} do
      query =
        from t in AuthToken,
          where: t.user_id == ^user.id,
          where: t.resource == ^"old_token"

      refute Repo.one(query)
    end

    test "should not remove the recent token", %{user: user} do
      query =
        from t in AuthToken,
          where: t.user_id == ^user.id,
          where: t.resource == ^"new_token"

      assert Repo.one(query)
    end
  end

  describe "clean_expired_invite_codes" do
    setup %{user: user} do
      old_code = User.make_invite_code(user)
      new_code = User.make_invite_code(user)

      invitation = Repo.get_by(InviteCode, code: old_code)
      ts = Timex.shift(invitation.created_at, weeks: -6)

      invitation
      |> Ecto.Changeset.change(created_at: ts)
      |> Repo.update!()

      {:ok, result} = Cleaner.clean_expired_invite_codes()

      {:ok, old_code: old_code, new_code: new_code, result: result}
    end

    test "should return the number of codes removed", %{result: result} do
      assert result == 1
    end

    test "should remove the old code", %{user: user, old_code: code} do
      query =
        from c in InviteCode,
          where: c.user_id == ^user.id,
          where: c.code == ^code

      refute Repo.one(query)
    end

    test "should not remove the recent code", %{user: user, new_code: code} do
      query =
        from c in InviteCode,
          where: c.user_id == ^user.id,
          where: c.code == ^code

      assert Repo.one(query)
    end
  end

  describe "clean_pending_tros_files" do
    setup %{user: user} do
      # Pending file, recent create date
      new_pending = Factory.insert(:tros_metadata, user: user, ready: false)

      # Pending file, older create date
      old_pending =
        Factory.insert(
          :tros_metadata,
          user: user,
          ready: false,
          created_at: Timestamp.shift(weeks: -2)
        )

      # Non-pending file, recent create date
      new_nonpending = Factory.insert(:tros_metadata, user: user, ready: true)

      # Non-pending file, older create date
      old_nonpending =
        Factory.insert(
          :tros_metadata,
          user: user,
          ready: true,
          created_at: Timestamp.shift(weeks: -2)
        )

      {:ok, result} = Cleaner.clean_pending_tros_files()

      {:ok,
       new_pending: new_pending,
       old_pending: old_pending,
       new_nonpending: new_nonpending,
       old_nonpending: old_nonpending,
       result: result}
    end

    test "should return the number of metadatas removed", %{result: result} do
      assert result == 1
    end

    test "should remove the old, pending metadata", ctx do
      refute Repo.get(Metadata, ctx.old_pending.id)
    end

    test "should not remove the recent, pending metadata", ctx do
      assert Repo.get(Metadata, ctx.new_pending.id)
    end

    test "should not remove the old, non-pending metadata", ctx do
      assert Repo.get(Metadata, ctx.old_nonpending.id)
    end

    test "should not remove the recent, non-pending metadata", ctx do
      assert Repo.get(Metadata, ctx.new_nonpending.id)
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

  defp setup_file_metadata(%{user: user}) do
    md = Factory.insert(:tros_metadata, user: user)
    {:ok, md: md, file_url: TROS.make_url(md.id)}
  end

  defp setup_bot_item_image_links(ctx) do
    bad_url = TROS.make_url(ID.new())
    bot = Factory.insert(:bot, user: ctx.user)

    good_with_content =
      Factory.insert(
        :item,
        bot: bot,
        user: ctx.user,
        image: true,
        stanza:
          item_stanza(
            image: ctx.file_url,
            content: "testing"
          )
      )

    bad_with_content =
      Factory.insert(
        :item,
        bot: bot,
        user: ctx.user,
        image: true,
        stanza:
          item_stanza(
            image: bad_url,
            content: "testing"
          )
      )

    good_no_content =
      Factory.insert(
        :item,
        bot: bot,
        user: ctx.user,
        image: true,
        stanza: item_stanza(image: ctx.file_url)
      )

    bad_no_content =
      Factory.insert(
        :item,
        bot: bot,
        user: ctx.user,
        image: true,
        stanza: item_stanza(image: bad_url)
      )

    only_content =
      Factory.insert(
        :item,
        bot: bot,
        user: ctx.user,
        image: false,
        stanza: item_stanza(content: "testing")
      )

    {:ok, result} = Cleaner.clean_bot_item_image_links(true)

    {:ok,
     bot: bot,
     good_with_content: good_with_content,
     bad_with_content: bad_with_content,
     good_no_content: good_no_content,
     bad_no_content: bad_no_content,
     only_content: only_content,
     result: result}
  end

  describe "clean_bot_item_image_links" do
    setup :setup_file_metadata
    setup :setup_bot_item_image_links

    test "should return the number of images purged", %{result: result} do
      assert result == 2
    end
  end

  describe "clean_bot_item_image_links when the item stanza has content" do
    setup :setup_file_metadata
    setup :setup_bot_item_image_links

    test "should not delete an item with an invalid image link", ctx do
      assert Repo.one(
               Item
               |> where(id: ^ctx.bad_with_content.id)
               |> where(bot_id: ^ctx.bot.id)
             )
    end

    test "should remove the invalid image link from the item", ctx do
      stanza =
        Item
        |> where(id: ^ctx.bad_with_content.id)
        |> where(bot_id: ^ctx.bot.id)
        |> select([i], i.stanza)
        |> Repo.one()

      assert xpath(stanza, ~x"/entry/image/text()"S) == ""
    end

    test "should not change a stanza with valid image link", ctx do
      stanza =
        Item
        |> where(id: ^ctx.good_with_content.id)
        |> where(bot_id: ^ctx.bot.id)
        |> select([i], i.stanza)
        |> Repo.one()

      assert stanza == ctx.good_with_content.stanza
    end

    test "should not change a stanza with no image link", ctx do
      stanza =
        Item
        |> where(id: ^ctx.only_content.id)
        |> where(bot_id: ^ctx.bot.id)
        |> select([i], i.stanza)
        |> Repo.one()

      assert stanza == ctx.only_content.stanza
    end
  end

  describe "clean_bot_item_image_links when the item stanza has no content" do
    setup :setup_file_metadata
    setup :setup_bot_item_image_links

    test "should delete an item with an invalid image link", ctx do
      refute Repo.one(
               Item
               |> where(id: ^ctx.bad_no_content.id)
               |> where(bot_id: ^ctx.bot.id)
             )
    end

    test "should not delete an item with a valid image link", ctx do
      assert Repo.one(
               Item
               |> where(id: ^ctx.good_no_content.id)
               |> where(bot_id: ^ctx.bot.id)
             )
    end
  end

  describe "clean_bot_image_links" do
    setup :setup_file_metadata

    setup %{user: user, file_url: file_url} do
      invalid_bot = Factory.insert(:bot, user: user)
      valid_bot = Factory.insert(:bot, user: user, image: file_url)

      {:ok, result} = Cleaner.clean_bot_image_links(true)
      {:ok, result: result, invalid_bot: invalid_bot, valid_bot: valid_bot}
    end

    test "should return the number of images nillified", %{result: result} do
      assert result == 1
    end

    test "should nillify non-existing images", ctx do
      refute Repo.one(
               Bot
               |> where(id: ^ctx.invalid_bot.id)
               |> select([b], b.image)
             )
    end

    test "should leave existing images", ctx do
      image =
        Bot
        |> where(id: ^ctx.valid_bot.id)
        |> select([b], b.image)
        |> Repo.one()

      assert image == ctx.file_url
    end
  end

  describe "clean_user_avatar_links" do
    setup :setup_file_metadata

    setup %{file_url: file_url} do
      valid_user = Factory.insert(:user, avatar: file_url)
      {:ok, result} = Cleaner.clean_user_avatar_links(true)
      {:ok, result: result, valid_user: valid_user}
    end

    test "should return the number of avatars nillified", %{result: result} do
      assert result == 1
    end

    test "should nillify non-existing avatars", ctx do
      refute Repo.one(
               User
               |> where(id: ^ctx.user.id)
               |> select([u], u.avatar)
             )
    end

    test "should leave existing avatars", ctx do
      avatar =
        User
        |> where(id: ^ctx.valid_user.id)
        |> select([u], u.avatar)
        |> Repo.one()

      assert avatar == ctx.file_url
    end
  end

  describe "clean_pending_users" do
    setup do
      # Pending user, recent create date
      new_pending = Factory.insert(:user, handle: nil)

      # Pending user, older create date
      old_pending =
        Factory.insert(
          :user,
          handle: nil,
          created_at: Timestamp.shift(days: -2)
        )

      # Non-pending user, recent create date
      new_nonpending = Factory.insert(:user)

      # Non-pending user, older create date
      old_nonpending =
        Factory.insert(:user, created_at: Timestamp.shift(days: -2))

      {:ok, result} = Cleaner.clean_pending_users()

      {:ok,
       new_pending: new_pending,
       old_pending: old_pending,
       new_nonpending: new_nonpending,
       old_nonpending: old_nonpending,
       result: result}
    end

    test "should return the number of users removed", %{result: result} do
      assert result == 1
    end

    test "should remove the old, pending user", ctx do
      refute Repo.get(User, ctx.old_pending.id)
    end

    test "should not remove the recent, pending user", ctx do
      assert Repo.get(User, ctx.new_pending.id)
    end

    test "should not remove the old, non-pending user", ctx do
      assert Repo.get(User, ctx.old_nonpending.id)
    end

    test "should not remove the recent, non-pending user", ctx do
      assert Repo.get(User, ctx.new_nonpending.id)
    end
  end

  defp get_by_token(token) do
    Repo.one(from PushToken, where: [token: ^token])
  end

  describe "clean_invalid_push_tokens" do
    setup do
      # Invalid token, recent update date
      new_invalid = Factory.insert(:push_token, valid: false)

      # Invalid token, older update date
      old_invalid =
        Factory.insert(
          :push_token,
          valid: false,
          enabled_at: Timestamp.shift(weeks: -3)
        )

      # Valid token, recent update date
      new_valid = Factory.insert(:push_token)

      # Valid token, older update date
      old_valid =
        Factory.insert(:push_token, enabled_at: Timestamp.shift(weeks: -3))

      {:ok, result} = Cleaner.clean_invalid_push_tokens()

      {:ok,
       new_invalid: new_invalid,
       old_invalid: old_invalid,
       new_valid: new_valid,
       old_valid: old_valid,
       result: result}
    end

    test "should return the number of push tokens removed", %{result: result} do
      assert result == 1
    end

    test "should remove the old, invalid push token", ctx do
      refute get_by_token(ctx.old_invalid.token)
    end

    test "should not remove the recent, invalid push token", ctx do
      assert get_by_token(ctx.new_invalid.token)
    end

    test "should not remove the old, valid push token", ctx do
      assert get_by_token(ctx.old_valid.token)
    end

    test "should not remove the recent, valid push token", ctx do
      assert get_by_token(ctx.new_valid.token)
    end
  end

  describe "clean_notification_logs" do
    setup %{user: user} do
      new = Factory.insert(:push_log, user: user)

      old =
        Factory.insert(
          :push_log,
          user: user,
          created_at: Timestamp.shift(months: -2)
        )

      {:ok, result} = Cleaner.clean_notification_logs()
      {:ok, result: result, new: new, old: old}
    end

    test "should return the number of log entries removed", %{result: result} do
      assert result == 1
    end

    test "should remove the old log entry", ctx do
      refute Repo.get(PushLog, ctx.old.id)
    end

    test "should not remove the recent log entry", ctx do
      assert Repo.get(PushLog, ctx.new.id)
    end
  end
end
