use ExGuard.Config

"tests"
|> guard(run_on_start: true)
|> command("mix test --color")
|> watch({~r{lib/(?<dir>.+)/(?<file>.+).ex$},
    fn (m) -> "test/#{m["dir"]}/#{m["file"]}_test.exs" end})
|> watch({~r{test/(?<dir>.+)/(?<file>.+)_test.exs$},
    fn (m) -> "test/#{m["dir"]}/#{m["file"]}_test.exs" end})
|> watch(~r{\.(erl|ex|exs|eex|xrl|yrl)\z}i)
|> ignore(~r|lib/.+/\.\#|)
|> ignore(~r|test/.+/\.\#|)
