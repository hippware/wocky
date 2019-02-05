defmodule WockyAPI.GraphQL.NotificationTest do
  use WockyAPI.GraphQLCase, async: true

  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.User.Notification

  setup do
    {:ok, user: Factory.insert(:user)}
  end

  describe "notification retrieval" do
    setup ctx do
      notifications =
        [
          :bot_item_notification,
          :geofence_event_notification,
          :bot_invitation_notification,
          :bot_invitation_response_notification,
          :user_invitation_notification
        ]
        |> Enum.map(
          &Factory.insert(&1, user: ctx.user, other_user: Factory.insert(:user))
        )

      {:ok, notifications: notifications}
    end

    @query """
    query ($first: Int, $last: Int,
           $beforeId: AInt, $afterId: AInt,
           $types: [NotificationType]) {
      notifications (first: $first, last: $last,
                     beforeId: $beforeId, afterId: $afterId, types: $types) {
        edges {
          node {
            id
            data {
              __typename
            }
            created_at
          }
        }
      }
    }
    """
    test "get last three notifications", %{user: user} do
      result = run_query(@query, user, %{"first" => 3})

      refute has_errors(result)

      assert %{
               data: %{
                 "notifications" => %{
                   "edges" => [
                     %{
                       "node" => %{
                         "created_at" => _,
                         "data" => %{
                           "__typename" => "UserInvitationNotification"
                         },
                         "id" => _
                       }
                     },
                     %{
                       "node" => %{
                         "created_at" => _,
                         "data" => %{
                           "__typename" => "BotInvitationResponseNotification"
                         },
                         "id" => _
                       }
                     },
                     %{
                       "node" => %{
                         "created_at" => _,
                         "data" => %{
                           "__typename" => "BotInvitationNotification"
                         },
                         "id" => _
                       }
                     }
                   ]
                 }
               }
             } = result
    end

    test "get after a particular id", shared do
      pivot_id = to_string(Enum.at(shared.notifications, 2).id)
      expected_id = to_string(Enum.at(shared.notifications, 3).id)

      result =
        run_query(@query, shared.user, %{"afterId" => pivot_id, "last" => 1})

      refute has_errors(result)

      assert %{
               data: %{
                 "notifications" => %{
                   "edges" => [
                     %{
                       "node" => %{
                         "created_at" => _,
                         "data" => %{
                           "__typename" => "BotInvitationResponseNotification"
                         },
                         "id" => ^expected_id
                       }
                     }
                   ]
                 }
               }
             } = result
    end

    test "get before a particular id", shared do
      pivot_id = to_string(Enum.at(shared.notifications, 2).id)
      expected_id = to_string(Enum.at(shared.notifications, 1).id)

      result =
        run_query(@query, shared.user, %{"beforeId" => pivot_id, "first" => 1})

      refute has_errors(result)

      assert %{
               data: %{
                 "notifications" => %{
                   "edges" => [
                     %{
                       "node" => %{
                         "created_at" => _,
                         "data" => %{
                           "__typename" => "GeofenceEventNotification"
                         },
                         "id" => ^expected_id
                       }
                     }
                   ]
                 }
               }
             } = result
    end

    test "filter by type", shared do
      result =
        run_query(@query, shared.user, %{
          "first" => 10,
          "types" => [
            "BOT_INVITATION_NOTIFICATION",
            "BOT_INVITATION_RESPONSE_NOTIFICATION"
          ]
        })

      expected_id = to_string(Enum.at(shared.notifications, 3).id)
      expected_id2 = to_string(Enum.at(shared.notifications, 2).id)

      refute has_errors(result)

      assert %{
               data: %{
                 "notifications" => %{
                   "edges" => [
                     %{
                       "node" => %{
                         "created_at" => _,
                         "data" => %{
                           "__typename" => "BotInvitationResponseNotification"
                         },
                         "id" => ^expected_id
                       }
                     },
                     %{
                       "node" => %{
                         "created_at" => _,
                         "data" => %{
                           "__typename" => "BotInvitationNotification"
                         },
                         "id" => ^expected_id2
                       }
                     }
                   ]
                 }
               }
             } = result
    end
  end

  describe "delete" do
    setup ctx do
      n = Factory.insert(:geofence_event_notification, user: ctx.user)
      n2 = Factory.insert(:geofence_event_notification)
      {:ok, id: n.id, id2: n2.id}
    end

    @query """
    mutation ($id: AInt!) {
      notificationDelete (input: {id: $id}) {
        successful
        result
      }
    }
    """
    test "should delete a notification", ctx do
      result = run_query(@query, ctx.user, %{"id" => to_string(ctx.id)})

      assert result.data == %{
               "notificationDelete" => %{
                 "successful" => true,
                 "result" => true
               }
             }

      assert Repo.get(Notification, ctx.id) == nil
    end

    test "should not delete another user's notification", ctx do
      result = run_query(@query, ctx.user, %{"id" => to_string(ctx.id2)})

      assert result.data == %{
               "notificationDelete" => %{
                 "successful" => true,
                 "result" => true
               }
             }

      refute Repo.get(Notification, ctx.id) == nil
    end

    test "should not fail on non-existant notification", ctx do
      result =
        run_query(@query, ctx.user, %{"id" => to_string(:rand.uniform(100))})

      assert result.data == %{
               "notificationDelete" => %{
                 "successful" => true,
                 "result" => true
               }
             }
    end
  end
end
