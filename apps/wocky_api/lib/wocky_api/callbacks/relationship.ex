defmodule WockyAPI.Callbacks.Relationship do
  @moduledoc """
  Callbacks for roster item changes
  """

  use DawdleDB.Handler, type: Wocky.Contacts.Relationship

  alias Wocky.Repo.Hydrator
  alias WockyAPI.Resolvers.Contact

  def handle_insert(%{state: :friend} = new),
    do: send_update(new, :friend, new.share_type)

  def handle_insert(_new), do: :ok

  def handle_update(%{state: :friend} = new, %{state: state})
      when state != :friend do
    send_update(new, :friend, new.share_type)
  end

  def handle_update(_new, _old), do: :ok

  def handle_delete(%{state: :friend} = old),
    do: send_update(old, :none, :disabled)

  def handle_delete(_old), do: :ok

  defp send_update(item, relationship, share_type) do
    Hydrator.with_assocs(item, [:contact], fn rec ->
      Contact.notify_contact(rec, relationship, share_type)
    end)
  end
end
