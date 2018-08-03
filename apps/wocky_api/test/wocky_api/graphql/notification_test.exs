defmodule WockyAPI.GraphQL.NotificationTest do
  use WockyAPI.GraphQLCase, async: true

  alias Wocky.Repo.Factory

  describe "initial notification retrieval" do
    setup do
      user = Factory.insert(:user)

      [
        :bot_item_notification,
        :geofence_event_notification,
        :invitation_notification,
        :invitation_response_notification,
        :user_follow_notification
      ]
      |> Enum.each(
        &Factory.insert(&1, user: user, other_user: Factory.insert(:user))
      )

      {:ok, user: user}
    end

    test "get last three notifications", %{user: user} do
      query = """
      {
        notifications (first: 3) {
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

      result = run_query(query, user)

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
  end
end
