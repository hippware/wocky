defmodule Wocky.Bot.Invitation do
  @moduledoc "An invitation from a user to subscribe to a bot"

  use Wocky.Repo.Schema

  import Ecto.Query

  alias Ecto.Changeset
  alias Wocky.{Bot, Repo, User}
  alias __MODULE__, as: Invitation

  @foreign_key_type :binary_id
  schema "bot_invitations" do
    field :accepted, :boolean

    belongs_to :user, User
    belongs_to :invitee, User
    belongs_to :bot, Bot

    timestamps()
  end

  @type id :: integer
  @type t :: %Invitation{}

  @spec put(User.t(), Bot.t(), User.t()) :: {:ok, t()} | {:error, any()}
  def put(invitee, %Bot{id: bot_id, user_id: user_id}, %User{id: user_id}) do
    %Invitation{}
    |> changeset(%{
      user_id: user_id,
      bot_id: bot_id,
      invitee_id: invitee.id
    })
    |> Repo.insert(
      returning: true,
      on_conflict: :replace_all,
      conflict_target: [:user_id, :bot_id, :invitee_id]
    )
  end

  def put(_, _, _), do: {:error, :permission_denied}

  @spec get(id(), User.t()) :: nil | t()
  def get(id, requestor) do
    Invitation
    |> where(
      [i],
      i.id == ^id and
        (i.user_id == ^requestor.id or i.invitee_id == ^requestor.id)
    )
    |> Repo.one()
  end

  @spec respond(t(), boolean(), User.t()) :: {:ok, t()} | {:error, any()}
  def respond(
        %Invitation{invitee_id: invitee_id} = invitation,
        accepted?,
        %User{id: invitee_id}
      ) do
    invitation = Repo.preload(invitation, [:bot, :invitee])

    with {:ok, result} <- do_respond(invitation, accepted?),
         :ok <- Bot.subscribe(invitation.bot, invitation.invitee, true) do
      {:ok, result}
    end
  end

  def respond(_, _, _), do: {:error, :permission_denied}

  defp do_respond(invitation, accepted?) do
    invitation
    |> changeset(%{accepted: accepted?})
    |> Repo.update()
  end

  @spec delete(User.t(), User.t()) :: :ok
  def delete(user, invitee) do
    Invitation
    |> where([i], i.user_id == ^user.id and i.invitee_id == ^invitee.id)
    |> Repo.delete_all()

    :ok
  end

  @spec changeset(t(), map()) :: Changeset.t()
  defp changeset(struct, params) do
    struct
    |> cast(params, [:user_id, :bot_id, :invitee_id, :accepted])
    |> validate_required([:user_id, :bot_id, :invitee_id])
    |> foreign_key_constraint(:user_id)
    |> foreign_key_constraint(:bot_id)
    |> foreign_key_constraint(:invitee_id)
  end
end
