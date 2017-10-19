defmodule Wocky.Email do
  @moduledoc """
  Generates and sends emails for various situations
  """

  import Bamboo.Email

  alias Bamboo.MandrillHelper
  alias Wocky.Mailer
  alias Wocky.User

  @spec send_welcome_email(User.t) :: :ok
  def send_welcome_email(user) do
    new_email()
    |> to({User.full_name(user), user.email})
    |> from(Confex.get_env(:wocky, :welcome_email_from))
    |> subject(Confex.get_env(:wocky, :welcome_email_subject))
    |> MandrillHelper.put_param("global_merge_vars", make_merge_vars(user))
    |> MandrillHelper.template(Confex.get_env(:wocky, :welcome_email_template))
    |> Mailer.deliver_later
    :ok
  end

  defp make_merge_vars(user) do
    :wocky
    |> Confex.get_env(:welcome_field_mappings, [])
    |> Enum.map(
      fn({merge_field, user_field}) ->
          %{"name": merge_field, "content": Map.get(user, user_field)}
      end)
  end
end
