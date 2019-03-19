defmodule Wocky.Push.Backend do
  alias Wocky.Push

  @callback push(Push.t()) :: :ok
  @callback handle_response(Push.notification(), pid(), Push.t()) :: :ok
  @callback error_msg(term()) :: binary()
end
