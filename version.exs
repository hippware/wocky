defmodule WockyVersion do
  @doc "Generates a version number using the date and git metadata"
  @spec version :: String.t
  def version do
    %DateTime{year: year, month: month, day: day} = DateTime.utc_now
    "#{year}.#{month}.#{day}+r#{git_rev()}"
  end

  defp git_rev do
    {rev, _} = System.cmd("git", ["rev-parse", "--short", "HEAD"])
    case String.trim(rev) do
      "" -> "0"
      str -> str
    end
  end
end

IO.write WockyVersion.version
