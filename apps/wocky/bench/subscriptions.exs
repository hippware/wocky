alias Wocky.Repo.Factory
alias Wocky.User

no_bots = Factory.insert(:user)
bots_10 = Factory.insert(:user)
bots_100 = Factory.insert(:user)
bots_1000 = Factory.insert(:user)

Factory.insert_list(10, :bot, user: bots_10)
Factory.insert_list(100, :bot, user: bots_100)
Factory.insert_list(1000, :bot, user: bots_1000)

Benchee.run(
  %{
    no_bots: fn -> User.get_subscriptions(no_bots) end,
    bots_10: fn -> User.get_subscriptions(bots_10) end,
    bots_100: fn -> User.get_subscriptions(bots_100) end,
    bots_1000: fn -> User.get_subscriptions(bots_1000) end
  }
)

# get_subscriptions =
#   fn user ->
#     {:ok, uuid} = Ecto.UUID.dump(user.id)

#     Ecto.Adapters.SQL.query!(
#       Wocky.Repo,
#       """
#       SELECT b.id, b.title, b.location
#       FROM bots AS b
#       LEFT OUTER JOIN bot_subscriptions AS bs
#         ON (b.id = bs.bot_id) AND (bs.user_id = $1)
#       WHERE (b.pending = FALSE) AND (NOT (b.user_id IS NULL));
#       """,
#       [uuid]
#     )
#   end

# {:ok, pid} =
#   Postgrex.start_link(
#     hostname: "localhost",
#     username: "postgres",
#     database: "wocky_dev"
#   )

# query =
#   Postgrex.prepare!(
#     pid,
#     "get_subscriptions",
#     """
#     SELECT b.id, b.title
#     FROM bots AS b
#     LEFT OUTER JOIN bot_subscriptions AS bs
#       ON (b.id = bs.bot_id) AND (bs.user_id = $1)
#     WHERE (b.pending = FALSE) AND (NOT (b.user_id IS NULL));
#     """
#   )

# get_subscriptions =
#   fn user ->
#     {:ok, uuid} = Ecto.UUID.dump(user.id)
#     Postgrex.execute!(pid, query, [uuid])
#   end

# Benchee.run(
#   %{
#     no_bots: fn -> get_subscriptions.(no_bots) end,
#     bots_10: fn -> get_subscriptions.(bots_10) end,
#     bots_100: fn -> get_subscriptions.(bots_100) end,
#     bots_1000: fn -> get_subscriptions.(bots_1000) end
#   }
# )
