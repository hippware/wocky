defmodule WockyAPI.Views.ErrorViewTest do
  use WockyAPI.ConnCase, async: true

  # Bring render/3 and render_to_string/3 for testing custom views
  import Phoenix.View

  alias WockyAPI.Views.ErrorView

  test "render any other" do
    assert render(ErrorView, "404", []) == "Not Found"
  end
end
