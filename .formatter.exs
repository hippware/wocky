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
    # Ecto queries
    from: 2,
    field: 2,
    field: 3,
    belongs_to: 2,
    belongs_to: 3,
    has_many: 2,
    has_many: 3,
    execute: 1,

    defenum: 2,

    # Phoenix.Channel
    intercept: 1,

    # Phoenix.Router
    connect: 3,
    connect: 4,
    delete: 3,
    delete: 4,
    forward: 2,
    forward: 3,
    forward: 4,
    get: 3,
    get: 4,
    head: 3,
    head: 4,
    match: 4,
    match: 5,
    options: 3,
    options: 4,
    patch: 3,
    patch: 4,
    pipe_through: 1,
    plug: 1,
    plug: 2,
    post: 3,
    post: 4,
    put: 3,
    put: 4,
    resources: 2,
    resources: 3,
    resources: 4,
    trace: 4,

    # Phoenix.Controller
    action_fallback: 1,

    # Phoenix.Endpoint
    socket: 2,

    # Phoenix.Socket
    channel: 2,
    channel: 3,
    transport: 2,
    transport: 3,

    # Phoenix.ChannelTest
    assert_broadcast: 2,
    assert_broadcast: 3,
    assert_push: 2,
    assert_push: 3,
    assert_reply: 2,
    assert_reply: 3,
    assert_reply: 4,
    refute_broadcast: 2,
    refute_broadcast: 3,
    refute_push: 2,
    refute_push: 3,
    refute_reply: 2,
    refute_reply: 3,
    refute_reply: 4,

    # Phoenix.ConnTest
    assert_error_sent: 2,

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
