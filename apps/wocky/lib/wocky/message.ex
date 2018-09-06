defmodule Wocky.Message do
  @moduledoc """
  DB interface module for messages
  """

  use Wocky.Repo.Schema

  import Ecto.Query

  alias Ecto.Queryable
  alias Wocky.Repo
  alias Wocky.User

  @foreign_key_type :binary_id
  schema "messages" do
    field :incoming, :boolean
    field :message, :string

    belongs_to :user, User
    belongs_to :other_user, User
  end

  @type t :: %Message{}

  @spec get_query(User.t()) :: Queryable.t()
  def get_query(user) do
    user
    |> Ecto.assoc(:messages)
  end

  @spec get_query(User.t(), User.t()) :: Queryable.t()
  def get_query(user, other_user) do
    user
    |> Ecto.assoc(:messages)
    |> where([m], m.other_user_id == ^other_user.id)
  end

  # An unfortunate necessity for now while we're supporting the old MIM
  # archive tables. Callers will need to call `fix()` on their messages
  # to decode the message body
  @spec fix(t() | [t()]) :: t() | [t()]

  def fix(%Message{} = message), do: fix_message(message)
  def fix(messages), do: Enum.map(messages, &fix_message/1)

  defp fix_message(%Message{message: body} = message),
    do: %{
      message
      | message: body |> :erlang.binary_to_term() |> :exml.to_binary()
    }
end
