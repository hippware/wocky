defmodule Wocky.User.Presence do
  @moduledoc """
  This module provides the interface for user presence management in Wocky
  """

  alias Wocky.User
  alias Wocky.User.Presence.Manager

  defstruct [
    :status,
    :updated_at
  ]

  @type t :: %__MODULE__{
          status: status(),
          updated_at: DateTime.t()
        }

  @type status() :: :offline | :online

  @type callback() :: (User.t(), User.id() -> :ok)

  @spec register_callback(callback()) :: [callback()]
  def register_callback(callback) do
    original_callbacks = Confex.get_env(:wocky, :presence_callbacks, [])
    callbacks = Enum.concat(original_callbacks, [callback])

    Application.put_env(:wocky, :presence_callbacks, callbacks)
    original_callbacks
  end

  @spec reset_callbacks([callback()]) :: :ok
  def reset_callbacks(callbacks),
    do: Application.put_env(:wocky, :presence_callbacks, callbacks)

  @doc """
  Mark a user online and return a list of their currently-online followees
  """
  @spec connect(User.t()) :: [User.t()]
  def connect(user) do
    {:ok, manager} = Manager.acquire(user)

    manager
    |> Manager.online_contacts()
    |> Enum.map(&User.get_user/1)
    |> Enum.map(&add_presence(&1, :online))
  end

  @doc "Get the online status for a given user"
  @spec get(User.t(), User.t()) :: t()
  def get(user, requestor) do
    {:ok, manager} = Manager.acquire(requestor)
    Manager.get_presence(manager, user)
  end

  @doc """
  Publish given online status of a user (`contact`) to another user
  (`recipient`)
  """
  @spec publish(User.id(), User.t(), status()) :: :ok
  def publish(recipient_id, contact, status) do
    full_contact = add_presence(contact, status)

    :wocky
    |> Confex.get_env(:presence_callbacks, [])
    |> Enum.each(& &1.(full_contact, recipient_id))
  end

  @spec add_presence(User.t(), status()) :: User.t()
  def add_presence(user, status),
    do: Map.put(user, :presence, make_presence(status))

  @spec make_presence(status()) :: __MODULE__.t()
  def make_presence(status),
    do: %__MODULE__{status: status, updated_at: DateTime.utc_now()}
end
