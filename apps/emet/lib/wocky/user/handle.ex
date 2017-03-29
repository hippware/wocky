defmodule Wocky.User.Handle do
  @moduledoc "Logic for validating and setting a user's handle"

  alias Wocky.User

  @type t :: binary

  defp reserved_handles,
    do: Application.get_env(:wocky, :reserved_handles, [])

  @doc ""
  @spec check_reserved(t) :: {:ok, t} | {:error, :duplicate_handle}
  def check_reserved(handle) do
    if Enum.member?(reserved_handles(), String.downcase(handle)) do
      {:error, :duplicate_handle}
    else
      {:ok, handle}
    end
  end

  # WARNING: There is plenty of room for a race condition here
  # FIXME: We need a better/more reliable way to detect multiple handles
  @doc ""
  @spec check_duplicate(t) :: {:ok, t} | {:error, :duplicate_handle}
  def check_duplicate(handle) do
    # IO.puts "!!! Search handle: #{handle}"
    case User.search(:handle, handle) do
      [] ->
        # IO.puts "!!! no results!"
        {:ok, handle}
      result ->
        # IO.puts "!!! #{inspect result}"
        {:error, :duplicate_handle}
    end
  end
end
