defmodule Wocky.Errors do
  @moduledoc "Utility module for handling errors."

  require Logger

  alias Ecto.Changeset

  @doc """
  A helper that transform changeset errors to a map of messages.

      assert {:error, changeset} = Accounts.create_user(%{password: "short"})
      assert "password is too short" in errors_on(changeset).password
      assert %{password: ["password is too short"]} = errors_on(changeset)

  """
  @spec errors_on(Changeset.t()) :: map()
  def errors_on(changeset) do
    Changeset.traverse_errors(changeset, fn {message, opts} ->
      Enum.reduce(opts, message, fn {key, value}, acc ->
        String.replace(acc, "%{#{key}}", to_string(value))
      end)
    end)
  end

  @doc """
  Convert an error result to a string.

  An error result may be the second element in an `{:error, _}` tuple or an
  exception.
  """
  @spec error_to_string(any()) :: String.t()
  def error_to_string(%{message: reason}), do: reason
  def error_to_string(reason) when is_atom(reason), do: to_string(reason)
  def error_to_string(reason) when is_binary(reason), do: reason

  def error_to_string(%Changeset{} = reason) do
    reason
    |> errors_on()
    |> Enum.reduce("", fn {field, messages}, acc ->
      prefix = if acc == "", do: "", else: "\n"

      acc <> prefix <> to_string(field) <> ": " <> Enum.join(messages, "\n\t")
    end)
  end

  def error_to_string(reason) do
    if Exception.exception?(reason) do
      Exception.message(reason)
    else
      inspect(reason)
    end
  end

  @doc """
  Execute the passed function and log the result if it is an error.
  """
  @spec log_on_failure(String.t(), fun()) :: :ok
  def log_on_failure(label, fun) do
    case fun.() do
      :ok ->
        :ok

      {:ok, _} ->
        :ok

      {:error, reason} ->
        _ = Logger.error("#{label}: #{error_to_string(reason)}")
        :ok
    end
  end
end
