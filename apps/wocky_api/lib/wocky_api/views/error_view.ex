defmodule WockyAPI.Views.ErrorView do
  use WockyAPI, :view

  # If you want to customize a particular status code
  # for a certain format, you may uncomment below.
  # def render("500.html", _assigns) do
  #   "Internal Server Error"
  # end

  # By default, Phoenix returns the status message from
  # the template name. For example, "404.html" becomes
  # "Not Found".
  def template_not_found(template, %{message: message}) do
    if String.ends_with?(template, ".json") do
      %{errors: %{detail: message}}
    else
      message
    end
  end

  def template_not_found(template, _assigns) do
    if String.ends_with?(template, ".json") do
      %{
        errors: %{
          detail: Phoenix.Controller.status_message_from_template(template)
        }
      }
    else
      Phoenix.Controller.status_message_from_template(template)
    end
  end
end
