use ExGuard.Config

"tests"
|> guard(run_on_start: true)
|> command("mix test --color")
|> watch({~r{lib/(?<dir>.+)/(?<file>.+).ex$},
    fn (m) -> "test/#{m["dir"]}/#{m["file"]}_test.exs" end})
|> watch({~r{(?<path>test/.+/.+_test\.exs$)}, fn (m) -> "#{m["path"]}" end})
|> ignore(~r{(lib|test)/.+/\.\#})
|> notification(:off)

"credo"
|> guard(run_on_start: false)
|> command("mix credo --color --strict -a --format oneline")
|> watch({~r{(?<path>apps/.+/(lib|test)/.+\.exs?$)}, fn (m) -> "#{m["path"]}" end})
|> ignore(~r{(lib|test)/.+/\.\#})
|> notification(:auto)
