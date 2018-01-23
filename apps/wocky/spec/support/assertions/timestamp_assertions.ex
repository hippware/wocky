defmodule TimestampAssertions do
  def be_later_than(value), do: {BeLaterThanAssertion, value}
  def be_earlier_than(value), do: {BeEarlierThanAssertion, value}

  def match(subject, term, expected) do
    result = DateTime.compare(subject, term)
    {result == expected, result}
  end

  def success_message(subject, term, positive, type) do
    act = if positive, do: "", else: " not"
    "#{inspect(subject)} is#{act} #{type} than #{inspect(term)}."
  end

  def error_message(subject, [term], result, positive, type) do
    if positive do
      "Expected #{inspect(subject)} to be #{type} than #{inspect(term)} but it was #{
        str(result)
      } instead."
    else
      "Expected #{inspect(subject)} not to be #{type} than #{term} but it was."
    end
  end

  defp str(:gt), do: "later than"
  defp str(:lt), do: "earlier than"
  defp str(:eq), do: "equal to"
end

defmodule BeLaterThanAssertion do
  @moduledoc """
  Defines 'be_later_than' assertion.

  later |> should(be_later_than early)
  """
  use ESpec.Assertions.Interface

  defp match(subject, term) do
    TimestampAssertions.match(subject, term, :gt)
  end

  defp success_message(subject, term, _result, positive) do
    TimestampAssertions.success_message(subject, term, positive, "later")
  end

  defp error_message(subject, term, result, positive) do
    TimestampAssertions.error_message(subject, term, result, positive, "later")
  end
end

defmodule BeEarlierThanAssertion do
  @moduledoc """
  Defines 'be_earlier_than' assertion.

  early |> should(be_earlier_than later)
  """
  use ESpec.Assertions.Interface

  defp match(subject, term) do
    TimestampAssertions.match(subject, term, :lt)
  end

  defp success_message(subject, term, _result, positive) do
    TimestampAssertions.success_message(subject, term, positive, "earlier")
  end

  defp error_message(subject, term, result, positive) do
    TimestampAssertions.error_message(
      subject,
      term,
      result,
      positive,
      "earlier"
    )
  end
end
