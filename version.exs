defmodule WockyVersion do
  @doc "Generates a version number using the date and git metadata"
  @spec version :: String.t
  def version do
    "#{date_str()}+#{git_rev_count()}.#{git_rev()}"
  end

  defp date_str do
    %DateTime{year: year, month: month, day: day} = DateTime.utc_now
    "#{year}.#{month}.#{day}"
  end

  defp git_rev_count do
    {revs, _} = System.cmd("git", ["rev-list", "HEAD"])

    revs
    |> String.split("\n")
    |> length
  end

  defp git_rev do
    {rev, _} = System.cmd("git", ["rev-parse", "--short", "HEAD"])
    String.strip(rev)
  end
end

IO.write WockyVersion.version
