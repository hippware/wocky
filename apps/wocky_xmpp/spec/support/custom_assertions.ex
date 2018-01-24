defmodule CustomAssertions do
  def cause_exit(), do: {CauseExitAssertion, []}
  @moduledoc "Custom Espec assertions"

  def cause_exit(term), do: {CauseExitAssertion, [term]}
end
