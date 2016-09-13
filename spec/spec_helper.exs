ESpec.configure fn(config) ->
  config.before fn(tags) ->
    # System.put_env("WOCKY_MINIMAL", "1")
    # Application.ensure_started(:stringprep)
    # :wocky_app.start
    {:shared, hello: :world, tags: tags}
  end

  config.finally fn(_shared) ->
    # :wocky_app.stop
    :ok
  end
end
