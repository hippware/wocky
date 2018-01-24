defmodule ChangesetAssertions do
  @moduledoc "Custom Ecto changeset assertions for Espec"

  def be_valid, do: {BeValidAssertion, []}
  def have_errors(value), do: {HaveErrorsAssertion, value}
end
