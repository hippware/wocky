defmodule Wocky.Account.User do
  @moduledoc "Schema and API for working with users."

  use Wocky.Repo.Schema

  alias Wocky.Account.Avatar
  alias Wocky.Account.ClientVersion
  alias Wocky.Contacts.Relationship
  alias Wocky.Messaging.Conversation
  alias Wocky.Messaging.Message
  alias Wocky.Notifier.InBand.Notification
  alias Wocky.Notifier.Push.Token, as: PushToken
  alias Wocky.POI.Bot
  alias Wocky.Presence
  alias Wocky.Relation.Invitation
  alias Wocky.Relation.Subscription
  alias Wocky.Repo
  alias Wocky.TROS.Metadata, as: TROSMetadata
  alias Wocky.UserInvite.InviteCode

  @primary_key {:id, :binary_id, autogenerate: false}
  schema "users" do
    # Unique ID of the currently logged-in device.
    # I am not sure if this is the right place to track this information,
    # but it is currently the path of least resistance.
    field :device, :string, virtual: true
    # The external auth provider
    field :provider, :string
    # The user ID received from the provider
    field :external_id, :string
    # User handle (as seen by other users)
    field :handle, :string
    # TROS URL of file containing user's avatar
    field :image_url, :string
    # User's name
    field :name, :string, default: ""
    # The user's phone number
    field :phone_number, :string
    # (also from auth provider)
    # User's email address
    field :email, :string
    # User's tagline
    field :tagline, :string
    # Password hash
    field :password, :string
    # Client data blob (opaque to server)
    field :client_data, :string
    field :pass_details, :string
    field :roles, {:array, :string}, default: []
    field :welcome_sent, :boolean
    field :smss_sent, :integer
    field :bot_created, :boolean
    field :transient, :boolean

    field :presence, :any, virtual: true

    timestamps()

    has_many :bots, Bot
    has_many :client_versions, ClientVersion
    has_many :conversations, Conversation
    has_many :invite_codes, InviteCode
    has_many :notifications, Notification
    has_many :push_tokens, PushToken
    has_many :received_invitations, Invitation, foreign_key: :invitee_id
    has_many :received_messages, Message, foreign_key: :recipient_id
    has_many :relationships, Relationship
    has_many :sent_invitations, Invitation
    has_many :sent_messages, Message, foreign_key: :sender_id
    has_many :tros_metadatas, TROSMetadata

    many_to_many :bot_invitations, Bot,
      join_through: Invitation,
      join_keys: [invitee_id: :id, bot_id: :id]

    many_to_many :bot_subscriptions, Bot, join_through: Subscription
  end

  @type id :: String.t()
  @type device :: String.t()
  @type provider :: String.t()
  @type external_id :: String.t()
  @type phone_number :: String.t()
  @type handle :: String.t()
  @type role :: String.t()

  @type t :: %__MODULE__{
          id: id,
          handle: nil | handle(),
          image_url: nil | String.t(),
          name: String.t(),
          email: nil | String.t(),
          tagline: nil | String.t(),
          provider: nil | provider(),
          external_id: nil | external_id(),
          phone_number: nil | phone_number(),
          roles: [role()],
          welcome_sent: boolean(),
          smss_sent: integer(),
          bot_created: boolean(),
          presence: nil | Presence.t(),
          transient: boolean()
        }

  @type tid :: t() | id()

  @update_fields [
    :handle,
    :image_url,
    :name,
    :phone_number,
    :email,
    :tagline,
    :roles,
    :external_id,
    :provider,
    :smss_sent,
    :bot_created,
    :client_data,
    :transient
  ]

  @min_handle_len 3
  @max_handle_len 16
  @max_name_len 65

  @spec id(tid()) :: id()
  def id(%__MODULE__{id: id}), do: id
  def id(id), do: id

  @spec hydrate(tid()) :: t() | nil
  def hydrate(%__MODULE__{} = user), do: user
  def hydrate(id), do: Repo.get(__MODULE__, id)

  @spec changeset(t(), map()) :: Changeset.t()
  def changeset(user, params) do
    user
    |> cast(params, @update_fields)
    |> validate_required(:phone_number)
    |> validate_change(:email, &validate_email/2)
    |> validate_length(:handle, min: @min_handle_len, max: @max_handle_len)
    |> validate_change(:handle, &validate_handle/2)
    |> validate_length(:name, max: @max_name_len)
    |> validate_change(:name, &validate_name/2)
    |> validate_change(:image_url, &validate_avatar(&1, user, &2))
    |> unique_constraint(:handle, name: :users_lower_handle_index)
    |> prepare_changes(fn changeset ->
      Avatar.maybe_delete_existing(changeset.changes[:image_url], user)
      changeset
    end)
  end

  defp validate_email(:email, email) do
    if EmailChecker.valid?(email) do
      []
    else
      [email: "address is invalid"]
    end
  end

  defp validate_handle(:handle, handle) do
    reserved =
      Enum.any?(
        reserved_handles(),
        &Regex.match?(~r/.*#{&1}.*/, String.downcase(handle))
      )

    cond do
      reserved ->
        [handle: "unavailable"]

      Regex.run(~r/[a-zA-Z0-9_]+/, handle) != [handle] ->
        [handle: "invalid characters"]

      true ->
        []
    end
  end

  defp reserved_handles, do: Application.get_env(:wocky, :reserved_handles, [])

  defp validate_name(field, name) do
    if Regex.run(validation_regex(), name) != [name] do
      [{field, "invalid characters"}]
    else
      []
    end
  end

  # regexs implementing the rules at
  # https://github.com/hippware/tr-wiki/wiki/
  #     User-fields-validation-discussion#first-name-last-name
  #
  # The ?! and ?<! blocks provide negated lookaround checks for characters
  # at the start and end of the string without consuming them.
  # \p{L*} cover the three unicode categories we accept (lowercase, uppercase,
  # and other characters). \p{Z} maintains any whitespace in our search term
  # (which is removed later when we split the tokens).
  defp validation_regex do
    ~r|(?![ \-0-9])[\p{Ll}\p{Lu}\p{Lo} \-\.'0-9]*(?<![ \-])|u
  end

  defp validate_avatar(:image_url, user, avatar) do
    case Avatar.validate(user, avatar) do
      {:ok, _} ->
        []

      {:error, :not_found} ->
        [image_url: "does not exist"]

      {:error, :invalid_file} ->
        [image_url: "has an invalid file name (must be UUID)"]

      {:error, :invalid_url} ->
        [image_url: "is an invalid file URL"]

      {:error, :not_file_owner} ->
        [image_url: "is not owned by the user"]
    end
  end
end

defimpl FunWithFlags.Actor, for: Wocky.Account.User do
  @impl true
  def id(%Wocky.Account.User{id: id}), do: "user:#{id}"
end

defimpl FunWithFlags.Group, for: Wocky.Account.User do
  @impl true
  def in?(%Wocky.Account.User{email: email}, :hippware) do
    email && String.ends_with?(email, "@hippware.com")
  end
end
