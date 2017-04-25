defmodule Wocky.Repo.Errors do
  @moduledoc "Helper functions for handling Ecto changeset errors."

  alias Ecto.Changeset

  @doc "Convert changeset errors into a map"
  @spec to_map(Changeset.t) :: map
  def to_map(changeset) do
    changeset.errors
    |> Enum.map(fn {field, detail} -> {field, render_detail(detail)} end)
    |> Enum.into(%{})
  end

  @doc "Render changeset error details to a string"
  @spec render_detail({binary, list} | binary) :: binary
  def render_detail({message, []}) do
    message
  end
  def render_detail({message, values}) do
    Enum.reduce values, message, fn {k, v}, acc ->
      String.replace(acc, "%{#{k}}", to_string(v))
    end
  end
  def render_detail(message) do
    message
  end

  @doc "Render an error map into a string with one line per error"
  @spec render_errors(map) :: binary
  def render_errors(errors) do
    errors
    |> Enum.map(fn {f, d} -> render_error(f, d) end)
    |> Enum.join("\r")
  end

  @doc "Render an string with field and validation error message"
  @spec render_error(atom, binary) :: binary
  def render_error(field, message) do
    "#{field |> to_string |> String.capitalize} #{message}."
  end
end
