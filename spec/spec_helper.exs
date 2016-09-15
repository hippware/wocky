ESpec.configure fn (config) ->
  config.before fn (tags) ->
    {:ok, tags: tags}
  end

  config.finally fn (_shared) ->
    :ok
  end
end
