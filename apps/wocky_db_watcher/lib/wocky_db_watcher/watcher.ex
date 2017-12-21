defmodule WockyDBWatcher.Watcher do
  @moduledoc """
  This module implementes the GenStage producer for DB callback events.
  Tables that you want events for must have appropriate triggers and functions
  created. @see Wocky.Repo.Migration.Utils
  """

  defmodule State do
    @moduledoc "The state of the watcher"

    defstruct [
      :ref,
      :channel,
      :object,
      :action
    ]
  end

  defstruct [
    :action,
    :object,
    :old,
    :new
  ]

  use GenStage

  require Logger

  alias Ecto.Changeset
  alias Poison.Parser
  alias Postgrex.Notifications
  alias WockyDBWatcher.Watcher.State

  @type action :: :insert | :update | :delete
  @type t :: %__MODULE__{}

  @spec start_link(module, action | [action]) :: {:ok, pid} | [{:ok, pid}]
  def start_link(object, actions) when is_list(actions) do
    Enum.map(actions, &start_link(object, &1))
  end

  def start_link(object, action) do
    GenStage.start_link(__MODULE__, [object, action],
                        name: name(object, action))
  end

  def name(object, action) do
    {:global,
      String.to_atom("#{table(object)}_#{Atom.to_string(action)}_db_watcher")}
  end

  defp table(object), do: object.__schema__(:source)

  def init([object, action]) do
    {:ok, pid} =
    :wocky_db_watcher
    |> Confex.fetch_env!(Wocky.Repo)
    |> Notifications.start_link

    channel = "#{table(object)}_#{Atom.to_string(action)}s"
    ref = Notifications.listen!(pid, channel)

    {:producer_consumer,
      %State{ref: ref,
             channel: channel,
             object: object,
             action: action}}
  end

  def handle_info({:notification, _, ref, channel, payload},
                  %State{ref: ref, channel: channel,
                         action: :insert, object: object} = state) do
    j = parse(payload)

    {:noreply,
      [%__MODULE__{
        action: :insert,
        object: object,
        new: make_struct(j.new, object)}],
     state}
  end

  def handle_info({:notification, _, ref, channel, payload},
                  %State{ref: ref, channel: channel,
                         action: :update, object: object} = state) do
    j = parse(payload)

    {:noreply,
      [%__MODULE__{
        action: :update,
        object: object,
        old: make_struct(j.old, object),
        new: make_struct(j.new, object)
      }],
     state}
  end

  def handle_info({:notification, _, ref, channel, payload},
                  %State{ref: ref, channel: channel,
                         action: :delete, object: object} = state) do
    j = parse(payload)

    {:noreply,
      [%__MODULE__{
        action: :delete,
        object: object,
        old: make_struct(j.old, object)
      }],
     state}
  end

  def handle_info(_, state), do: {:noreply, state}

  defp parse(payload), do: Parser.parse!(payload, keys: :atoms!)

  defp make_struct(json, object) do
    object.__struct__
    |> Changeset.cast(json, object.__schema__(:fields))
    |> Changeset.apply_changes
  end
end
