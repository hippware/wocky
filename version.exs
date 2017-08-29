defmodule WockyVersion do
  @doc "Generates a version number using the date and git metadata"
  @spec version :: String.t
  def version do
    "1.0.#{git_rev_count()}-r#{git_rev()}"
  end

  defp git_rev_count do
    {revs, _} = System.cmd("git", ["rev-list", "HEAD"])

    revs
    |> String.split("\n")
    |> length
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
