# Hack to work around https://github.com/jeremyjh/dialyxir/issues/221:
defmodule Fix221 do
  def ignores(prefix) do
    Enum.map(
      primitives(),
      fn p ->
        m = Module.concat(prefix, p)
        {
          ":0:unknown_function Function #{inspect(m)}.__impl__/1 does not exist.",
          :unknown_function,
          0
        }
      end)
  end

  defp primitives, do:
    [
      Atom,
      BitString,
      Float,
      Function,
      Integer,
      List,
      Map,
      PID,
      Port,
      Reference,
      Tuple
    ]
end

[
  ~r|apps/wocky/lib/wocky/account/jwt/.*\.ex:.:contract_supertype|,
  {"apps/wocky/lib/wocky/repo/instrumenter.ex", :unmatched_return, 3},
  {"apps/wocky_api/lib/wocky_api/pipeline_instrumenter.ex", :unmatched_return, 3},
  {"apps/wocky_api/test/support/channel_helper.ex", :unmatched_return, 29}
] ++
  Fix221.ignores(Wocky.Push.Event) ++
  Fix221.ignores(Wocky.User.Notifier)
