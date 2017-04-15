ESpec.configure fn (config) ->
  config.before fn (tags) ->
    {:ok, tags: tags, server: "local.test"}
  end

  config.finally fn (_shared) ->
    :ok
  end
end
