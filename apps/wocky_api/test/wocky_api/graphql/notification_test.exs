defmodule WockyAPI.GraphQL.NotificationTest do
  use WockyAPI.GraphQLCase, async: true

  alias Wocky.Repo.Factory

  describe "initial notification retrieval" do
    setup do
      user = Factory.insert(:user)

      notifications =
        [
          :bot_item_notification,
          :geofence_event_notification,
          :invitation_notification,
          :invitation_response_notification,
          :user_follow_notification
        ]
        |> Enum.map(
          &Factory.insert(&1, user: user, other_user: Factory.insert(:user))
        )

      {:ok, user: user, notifications: notifications}
    end

    @query """
    query ($first: Int, $last: Int, $beforeId: AInt, $afterId: AInt) {
      notifications (first: $first, last: $last,
                     beforeId: $beforeId, afterId: $afterId) {
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
                         "data" => %{"__typename" => "UserFollowNotification"},
                         "id" => _
                       }
                     },
                     %{
                       "node" => %{
                         "created_at" => _,
                         "data" => %{
                           "__typename" => "InvitationResponseNotification"
                         },
                         "id" => _
                       }
                     },
                     %{
                       "node" => %{
                         "created_at" => _,
                         "data" => %{"__typename" => "InvitationNotification"},
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
                           "__typename" => "InvitationResponseNotification"
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
  end
end
