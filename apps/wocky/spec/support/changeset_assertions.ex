defmodule ChangesetAssertions do
  def be_valid, do: {BeValidAssertion, []}
  def have_errors(value), do: {HaveErrorsAssertion, value}
end
