[
  inputs: [
    "mix.exs",
    "config/*.{ex,exs}",
    "apps/*/mix.exs",
    "apps/*/{config,lib,test,spec}/**/*.{ex,exs}"
  ],
  line_length: 80,
  # In the future, we will be able to import DSL function names from
  # Phoenix, Ecto, and Espec. None of those libraries currently export,
  # but it is in the works for all of them.
  import_deps: [:absinthe, :phoenix, :ecto, :ecto_sql],
  locals_without_parens: [
    execute: 1,
    defenum: 2,

    # Absinthe / Kronky
    payload_object: 2,
    scope: 1
  ]
]
