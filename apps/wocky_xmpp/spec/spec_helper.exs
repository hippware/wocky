Code.require_file("spec/support/custom_assertions.ex")
Code.require_file("spec/support/assertions/cause_exit_assertion.ex")

ESpec.configure(fn config ->
  config.before(fn tags ->
    if tags[:sandbox], do: SandboxHelper.checkout()

    {:ok, tags: tags, server: "localhost"}
  end)

  config.finally(fn shared ->
    if shared.tags[:sandbox], do: SandboxHelper.checkin()

    :ok
  end)
end)

defmodule SandboxHelper do
  alias Ecto.Adapters.SQL.Sandbox

  defmacro __using__(_) do
    quote do
      before_all do
        SandboxHelper.mode(:manual)
      end

      after_all do
        SandboxHelper.mode(:auto)
      end
    end
  end

  def mode(mode) do
    Sandbox.mode(Wocky.Repo, mode)
  end

  def checkout do
    :ok =
      case Sandbox.checkout(Wocky.Repo) do
        :ok -> :ok
        {:already, :owner} -> :ok
        error -> error
      end
  end

  def checkin do
    Sandbox.checkin(Wocky.Repo, [])
  end
end

defmodule XMLHelper do
  defmacro __using__(_) do
    quote do
      import Record, only: [defrecordp: 2, extract: 2]
      require Record

      defrecordp :iq, extract(:iq, from_lib: "ejabberd/include/jlib.hrl")
      defrecordp :xmlel, extract(:xmlel, from_lib: "exml/include/exml.hrl")

      defrecordp :xmlcdata,
                 extract(:xmlcdata, from_lib: "exml/include/exml.hrl")
    end
  end
end

defmodule IQHandlerSpec do
  defmacro __using__(_) do
    quote do
      use XMLHelper
      use Wocky.JID

      @server_jid Wocky.JID.make("", "localhost")
    end
  end
end
