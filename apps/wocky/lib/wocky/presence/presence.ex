defmodule Wocky.Presence do
  @moduledoc """
  This module provides the interface for user presence management in Wocky
  """

  alias Wocky.Account
  alias Wocky.Account.User
  alias Wocky.Presence.Manager
  alias Wocky.Presence.PresenceEvent

  defstruct [
    :status,
    :updated_at
  ]

  @type t :: %__MODULE__{
          status: status(),
          updated_at: DateTime.t()
        }

  @type status() :: :offline | :online

  @doc """
  Mark a user connected and return a list of their currently-online followees
  """
  @spec connect(User.t()) :: [User.t()]
  def connect(user) do
    user
    |> Manager.register_handler()
    |> Manager.online_contacts()
    |> Enum.map(&Account.get_user/1)
    |> Enum.map(&add_presence(&1, :online))
  end

  @doc "Get the online status for a given user"
  @spec get(User.t(), User.t()) :: t()
  def get(user, requestor) do
    {:ok, manager} = Manager.acquire(requestor)
    Manager.get_presence(manager, user)
  end

  @spec set_status(User.t(), status()) :: :ok
  def set_status(user, status) do
    {:ok, manager} = Manager.acquire(user)
    Manager.set_status(manager, status)
  end

  @doc """
  Publish given online status of a user (`contact`) to another user
  (`recipient`)
  """
  @spec publish(User.id(), User.t(), status()) :: :ok
  def publish(recipient_id, contact, status) do
    full_contact = add_presence(contact, status)

    %PresenceEvent{contact: full_contact, recipient_id: recipient_id}
    |> Dawdle.signal(direct: true)
  end

  @spec add_presence(User.t(), status()) :: User.t()
  def add_presence(user, status),
    do: Map.put(user, :presence, make_presence(status))

  @spec make_presence(status()) :: __MODULE__.t()
  def make_presence(status),
    do: %__MODULE__{status: status, updated_at: DateTime.utc_now()}

  @spec register_socket(User.t(), pid()) :: :ok
  def register_socket(user, socket_pid) do
    {:ok, manager} = Manager.acquire(user)
    Manager.register_socket(manager, socket_pid)
  end

  @spec get_sockets(User.t()) :: [pid()]
  def get_sockets(user) do
    {:ok, manager} = Manager.acquire(user)
    Manager.get_sockets(manager)
  end

  @spec connected?(User.t()) :: boolean()
  def connected?(user), do: get_sockets(user) != []
end
