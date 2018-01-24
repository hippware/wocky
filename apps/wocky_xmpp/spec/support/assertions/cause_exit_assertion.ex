defmodule CauseExitAssertion do
  @moduledoc """
  Defines 'cause_exit' assertion.

  it do: expect(function).to cause_exit

  it do: expect(function).to cause_exit(exit_term)
  """
  use ESpec.Assertions.Interface

  defp match(subject, []) do
    subject.()
    {false, false}
  catch
    :exit, _ -> {true, false}
  end

  defp match(subject, [term]) do
    subject.()
    {false, {false, nil}}
  catch
    :exit, exit_term ->
      if exit_term == term do
        {true, term}
      else
        {false, term}
      end
  end

  defp success_message(subject, [], _result, positive) do
    act = if positive, do: "exits", else: "doesn't exit"
    "#{inspect(subject)} #{act}."
  end

  defp success_message(subject, [term], _result, positive) do
    act = if positive, do: "exits", else: "doesn't exit"
    "#{inspect(subject)} #{act} with `#{term}`."
  end

  defp error_message(subject, [], false, positive) do
    if positive do
      "Expected #{inspect(subject)} to exit, but it didn't."
    else
      "Expected #{inspect(subject)} not exit, but it did."
    end
  end

  defp error_message(subject, [term], err_term, positive) do
    if positive do
      case err_term do
        {false, nil} ->
          "Expected #{inspect(subject)} to exit with `#{term}, but it didn't exit."

        err_term ->
          "Expected #{inspect(subject)} to exit with `#{term}, but it exited with `#{
            err_term
          }` was raised instead."
      end
    else
      "Expected #{inspect(subject)} not to exit with `#{term}` exception, but it did."
    end
  end
end
