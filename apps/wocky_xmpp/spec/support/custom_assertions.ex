defmodule CustomAssertions do
  @moduledoc "Custom Espec assertions"

  def cause_exit, do: {CauseExitAssertion, []}
  def cause_exit(term), do: {CauseExitAssertion, [term]}
end
