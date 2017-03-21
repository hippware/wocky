
defmodule Wocky.Conversation do
  @moduledoc ""

  alias Wocky.Repo
  alias __MODULE__, as: Conversation

  @type id :: :mod_mam.message_id()

  @type t :: %Conversation{
    id:        id,
    server:    nil | binary,
    user:      nil | binary,
    other_jid: nil | binary,
    time:      nil | non_neg_integer,
    message:   nil | binary,
    outgoing:  nil | boolean
  }

  defstruct [
    :id,
    :server,
    :user,
    :other_jid,
    :time,
    :message,
    :outgoing
  ]

  @doc "Create a conversation object"
  @spec new(id, binary, binary, binary, integer, binary, boolean) :: t
  def new(id, server, user, other_jid, time, message, outgoing) do
    %Conversation{id: id,
                  server: server,
                  user: user,
                  other_jid: other_jid,
                  time: time,
                  message: message,
                  outgoing: outgoing}
    |> struct()
  end

  defp new(data) do
    %Conversation{}
    |> struct(data)
    |> Map.update!(:id, &String.to_integer/1)
    |> Map.update!(:time, &String.to_integer/1)
    |> Map.update!(:outgoing, &String.to_existing_atom/1)
  end

  @doc "Write a conversation record to the database"
  @spec put(t) :: :ok | {:error, term}
  def put(%Conversation{server: server} = conv) do
    conv |>
    Map.from_struct |>
    Repo.update("conversations", server, key(conv))
  end

  def find(user) do
    "conversations"
    |> Repo.search("user_register:\"#{user}\"")
    |> Enum.map(&Wocky.Repo.Doc.to_map/1)
    |> Enum.map(fn data -> new(data) end)
  end

  defp key(%Conversation{user: user, other_jid: other_jid}) do
    user <> "/" <> other_jid
  end

end
