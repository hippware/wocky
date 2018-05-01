defmodule WockyAPI.Plugs.AbsintheConnDataTest do
  use WockyAPI.ConnCase

  import WockyAPI.Plugs.AbsintheConnData

  describe ":load_graphql_context plug" do
    test "when current_user is assigned", context do
      conn =
        context.conn
        |> assign(:current_user, :foo)
        |> load_graphql_context

      assert  %{context: %{current_user: :foo}} = conn.private[:absinthe]
    end

    test "when current_user is not assigned", context do
      conn =
        context.conn
        |> load_graphql_context

      refute conn.private[:absinthe][:current_user]
    end
  end
end
