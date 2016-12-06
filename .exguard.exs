use ExGuard.Config

"specs"
|> guard(run_on_start: true)
|> command("mix espec")
|> watch({~r{lib/(?<dir>.+)/(?<file>.+).ex$},
    fn (m) -> "spec/#{m["dir"]}/#{m["file"]}_spec.exs" end})
|> watch(~r{\.(erl|ex|exs|eex|xrl|yrl)\z}i)
|> ignore(~r|db/migrations/|)

"compile"
|> guard(run_on_start: true)
|> command("mix compile")
|> watch(~r{\.(erl|ex|exs|eex|xrl|yrl)\z}i)
|> ignore(~r/_spec\.exs$/)
|> ignore(~r/_test\.exs$/)
|> notification(:off)

"credo"
|> guard(run_on_start: false)
|> command("mix credo")
|> watch(~r{lib/.*\.(ex|exs)\z}i)

"dogma"
|> guard(run_on_start: false)
|> command("mix dogma")
|> watch(~r{lib/.*\.(ex|exs)\z}i)
