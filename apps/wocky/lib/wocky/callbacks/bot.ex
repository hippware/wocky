defmodule Wocky.Callbacks.Bot do
  @moduledoc """
  Callbacks for DB bot changes
  """

  use DawdleDB.Handler, type: Wocky.Bot

  alias Wocky.Bot
  alias Wocky.Repo
  alias Wocky.Waiter

  def handle_insert(new) do
    %{user: user} = Repo.preload(new, [:user])

    if user != nil do
      Bot.subscribe(new, user)

      new
      |> Bot.sub_setup_event()
      |> Waiter.notify()
    end
  end
end
