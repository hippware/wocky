Code.require_file("spec/support/assertions/be_valid_assertion.ex")
Code.require_file("spec/support/assertions/have_errors_assertion.ex")
Code.require_file("spec/support/changeset_assertions.ex")

ESpec.configure fn (config) ->
  config.before fn (tags) ->
    {:ok, tags: tags, server: "local.test"}
  end

  config.finally fn (_shared) ->
    :ok
  end
end
