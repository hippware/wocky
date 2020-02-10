# Jason Encoder implementations for types that can end up in log fields
# (and thus are passed to ExJsonLogger) but are not handled by default.

defimpl Jason.Encoder, for: Function do
  @spec encode(function(), Jason.Encode.opts()) :: String.t()
  def encode(fun, _opts) do
    [?\", inspect(fun), ?\"]
  end
end

defimpl Jason.Encoder, for: Tuple do
  @spec encode(tuple(), Jason.Encode.opts()) :: String.t()
  def encode(tuple, _opts) do
    values =
      tuple
      |> Tuple.to_list()
      |> Enum.map(&inspect/1)
      |> Enum.join(", ")

    "\"{" <> values <> "}\""
  end
end
