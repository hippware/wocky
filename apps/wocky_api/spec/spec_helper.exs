ESpec.configure(fn config ->
  ESpec.Configuration.before(fn tags ->
    {:ok, tags: tags, server: "local.test"}
  end,
  config)

  ESpec.Configuration.finally(fn _shared ->
    :ok
  end,
  config)
end)
