defmodule WockyAPI.Metrics.QueryCounterTest do
  use Elixometer
  use WockyAPI.GraphQLCase, async: false

  import Eventually

  setup do
    user = Factory.insert(:user)
    {:ok, user: user}
  end

  describe "query count" do
    test "should count total queries made", %{user: user} do
      metric = "wocky.test.counters.graphql.requests.query.current_user"
      start_value = get_metric(metric)

      query = "{ currentUser { id } }"

      run_query(query, user)

      assert_eventually(get_metric(metric) == start_value + 1)
    end
  end

  defp get_metric(name) do
    {:ok, result} = get_metric_value(name)
    Keyword.get(result, :value)
  end
end
