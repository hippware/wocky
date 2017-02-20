defmodule Mix.Tasks.Lambda do
  use Mix.Task
  @moduledoc """
  Builds deployment packages for AWS lambda functions

  Mix config:
  ```
  def project do
    [
      # ...
      lambda: [
        packages: [{"path-to-src/function.py", function.zip},
                   {"path-to-other-src/fun2.py", fun2.zip}]
      ]
    ]
  end
  ```
  """

  @shortdoc "Compile and package a python script for AWS Lambda"
  def run(_) do
    {:ok, _} = Application.ensure_all_started(:porcelain)
    get_packages() |> Enum.each(&build_packages/1)
  end

  defp build_packages({sourcefile, targetfile}) do
    # Compilation check of the script
    compile_result = Porcelain.exec("python", ["-m", "py_compile", sourcefile])
    if compile_result.status != 0, do: raise("Failed to compile Python script")

    # Zip the file with no path data
    data = File.read!(sourcefile)
    zippedfile = sourcefile |>
                 Path.basename |>
                 String.to_charlist
    outfile = Mix.Project.build_path |>
              Path.join("lambda") |>
              Path.join(targetfile) |>
              String.to_charlist
    {:ok, _} = :zip.create(outfile, [{zippedfile, data}])
  end

  defp get_packages do
    Mix.Project.config[:lambda][:packages]
  end
end
