use ExGuard.Config

guard("specs", run_on_start: true)
|> command("mix espec")
|> watch({~r{lib/(?<dir>.+)/(?<file>.+).ex$}, fn (m) -> "spec/#{m["dir"]}/#{m["file"]}_spec.exs" end})
|> watch(~r{\.(erl|ex|exs|eex|xrl|yrl)\z}i)
|> ignore(~r|db/migrations/|)

# guard("compile", run_on_start: true)
# |> command("mix compile")
# |> watch(~r{\.(erl|ex|exs|eex|xrl|yrl)\z}i)
# |> ignore(~r/_spec\.exs$/)
# |> ignore(~r/_test\.exs$/)
# |> notification(:off)

# guard("credo", run_on_start: true)
# |> command("mix credo")
# |> watch(~r{lib/.*\.(ex|exs)\z}i)

# guard("dogma", run_on_start: true)
# |> command("mix dogma")
# |> watch(~r{lib/.*\.(ex|exs)\z}i)
