use ExGuard.Config

"specs"
|> guard(run_on_start: true)
|> command("mix espec")
|> watch({~r{lib/(?<dir>.+)/(?<file>.+).ex$},
    fn (m) -> "spec/#{m["dir"]}/#{m["file"]}_spec.exs" end})
|> watch({~r{spec/(?<dir>.+)/(?<file>.+)_spec.exs$},
    fn (m) -> "spec/#{m["dir"]}/#{m["file"]}_spec.exs" end})
|> watch(~r{\.(erl|ex|eex|xrl|yrl)\z}i)
|> ignore(~r|priv/repo/migrations/|)
|> ignore(~r|lib/.+/\.\#|)
|> ignore(~r|spec/.+/\.\#|)
