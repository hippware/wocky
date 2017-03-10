
defmodule Wocky.Conversation do
  @moduledoc ""

  alias Wocky.Repo
  alias Wocky.Repo.Object

  @type id :: :mod_mam.message_id()

  @type t :: %__MODULE__{
    id:        id,
    server:    nil | binary,
    user:      nil | binary,
    other_jid: nil | binary,
    time:      nil | integer,
    message:   nil | binary,
    outgoing:  nil | boolean
  }

  @enforce_keys [:id]
  defstruct [
    id:        nil,
    server:    nil,
    user:      nil,
    other_jid: nil,
    time:      nil,
    message:   nil,
    outgoing:  nil
  ]

  @doc "Create a conversation object"
  @spec new(id, binary, binary, binary, integer, binary, boolean) :: t
  def new(id, server, user, other_jid, time, message, outgoing) do
    %__MODULE__{id: id,
                server: server,
                user: user,
                other_jid: other_jid,
                time: time,
                message: message,
                outgoing: outgoing}
              |> struct()
  end

  defp new(id, server, data) do
    %__MODULE__{id: String.to_integer(id), server: server} |> struct(data)
  end

  @doc "Read a conversation record from the database"
  @spec read(id, binary) :: t | nil
  def read(id, server) do
    case Repo.find("conversation", server, id) do
      nil -> nil
      data -> new(id, server, data)
    end
  end

  @doc "Write a conversation record to the database"
  @spec write(t) :: :ok | {:error, term}
  def write(%__MODULE__{server: server} = conv) do
    conv |>
    Map.from_struct |>
    Repo.update("conversation", server, key(conv))
  end

  def find(user) do
    "conversation"
    |> Repo.search("user_register:\"#{user}\"")
    |> Enum.map(&Wocky.Repo.Doc.to_map/1)
    |> Enum.map(fn data -> new(data[:id], data[:server],
                               Map.drop(data, [:id, :server])) end)
  end

  defp key(%__MODULE__{user: user, other_jid: other_jid}) do
    user <> "/" <> other_jid
  end

end
