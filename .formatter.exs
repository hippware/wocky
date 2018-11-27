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
  import_deps: [:absinthe, :phoenix, :ecto],
  locals_without_parens: [
    execute: 1,
    defenum: 2,

    # Absinthe / Kronky
    payload_object: 2,
    scope: 1,

    # Espec

    ## Test setup
    before: 1,
    finally: 1,
    subject: :*,
    subject!: :*,
    let: :*,
    let!: :*,
    let_error: :*,
    let_error!: :*,
    let_ok: :*,
    let_ok!: :*,
    let_overridable: 1,

    ## Examples
    it_behaves_like: :*,
    it: :*,

    ## Assertions / Matchers
    assert: 1,
    refute: 1,
    assert_receive: 3,
    assert_received: 2,
    refute_receive: 3,
    refute_received: 2,
    eq: 1,
    eql: 1,

    ### be_*
    be: :*,
    be_function: :*,
    be_between: 2,
    be_struct: 1,

    ### have_*
    have: 1,
    have_any: 1,
    have_key: 1,
    have_value: 1,
    have_count: 1,
    have_first: 1,
    have_last: 1,
    have_length: 1,
    have_size: 1,
    have_errors: 1,

    ### match_*
    match: 1,
    match_pattern: 1,
    match_list: 1,

    # String matchers
    start_with: 1,
    end_with: 1,

    ### Error matchers
    raise_exception: :*,

    ## Mocking / Stubbing
    allow: 1,
    accept: 2,
    accepted: :*
  ]
]
