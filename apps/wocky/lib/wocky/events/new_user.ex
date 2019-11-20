defmodule Wocky.Events.NewUser do
  @moduledoc false

  alias Wocky.Account.User

  defstruct [
    :user
  ]

  @type t :: %__MODULE__{
          user: User.t()
        }
end

defimpl Wocky.Notifier.Email.Event, for: Wocky.Events.NewUser do
  alias Ecto.Changeset
  alias Wocky.Notifier.Email.WelcomeEmail
  alias Wocky.Repo

  @impl true
  def notify?(_event), do: Confex.get_env(:wocky, :send_welcome_email)

  @impl true
  def send(%{user: user}) do
    WelcomeEmail.send(user)

    _ =
      user
      |> Changeset.cast(%{welcome_sent: true}, [:welcome_sent])
      |> Repo.update()

    :ok
  end

  @impl true
  def ignore_block?(_event), do: false
end
