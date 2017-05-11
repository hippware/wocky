defmodule Wocky.JID do
  @moduledoc "TODO"

  import Record, only: [defrecord: 2]

  require Record

  defrecord :jid, [
    user:      "",
    server:    "",
    resource:  "",
    luser:     "",
    lserver:   "",
    lresource: ""
  ]

  @type user      :: binary
  @type server    :: binary
  @type resource  :: binary
  @type luser     :: binary
  @type lserver   :: binary
  @type lresource :: binary

  @type literal_jid :: binary

  @type jid :: record(:jid, user:      user,
                            server:    server,
                            resource:  resource,
                            luser:     luser,
                            lserver:   lserver,
                            lresource: lresource)

  @type t :: jid

  @sane_limit 1024

  defmacro __using__(_) do
    quote do
      alias unquote(__MODULE__)
      import unquote(__MODULE__), only: :macros
    end
  end

  @spec make(user, server, resource) :: t | :error
  def make(user, server, resource \\ "") do
    with {:ok, luser} <- nodeprep(user),
         {:ok, lserver} <- nameprep(server),
         {:ok, lresource} <- resourceprep(resource)
    do
      jid(user: user,
          server: server,
          resource: resource,
          luser: luser,
          lserver: lserver,
          lresource: lresource)
    else
      :error -> :error
    end
  end

  @spec equal?(t, t) :: boolean
  def equal?(jid(luser: luser, lserver: lserver, lresource: lres),
             jid(luser: luser, lserver: lserver, lresource: lres)), do: true
  def equal?(_, _), do: false

  @spec from_binary(literal_jid) :: t | :error
  def from_binary(j), do: binary_to_jid1(j, [])

  @spec binary_to_jid1(binary, [byte]) :: t | :error
  defp binary_to_jid1("@" <> _j, []), do: :error
  defp binary_to_jid1("/" <> _j, []), do: :error
  defp binary_to_jid1(<<>>, []), do: :error
  defp binary_to_jid1("@" <> j, n) do
    binary_to_jid2(j, Enum.reverse(n), [])
  end
  defp binary_to_jid1("/" <> j, n) do
    binary_to_jid3(j, [], Enum.reverse(n), [])
  end
  defp binary_to_jid1(<<c, j :: binary>>, n) do
    binary_to_jid1(j, [c | n])
  end
  defp binary_to_jid1(<<>>, n) do
    make("", to_string(Enum.reverse(n)), "")
  end

  # Only one "@" is admitted per JID
  @spec binary_to_jid2(binary, [byte], [byte]) :: t | :error
  defp binary_to_jid2("@" <> _j, _n, _s), do: :error
  defp binary_to_jid2("/" <> _j, _n, []), do: :error
  defp binary_to_jid2(<<>>, _n, []), do: :error
  defp binary_to_jid2("/" <> j, n, s) do
    binary_to_jid3(j, n, Enum.reverse(s), [])
  end
  defp binary_to_jid2(<<c, j :: binary>>, n, s) do
    binary_to_jid2(j, n, [c | s])
  end
  defp binary_to_jid2(<<>>, n, s) do
    make(to_string(n), to_string(Enum.reverse(s)), "")
  end

  @spec binary_to_jid3(binary, [byte], [byte], [byte]) :: jid | :error
  defp binary_to_jid3(<<c, j :: binary>>, n, s, r) do
    binary_to_jid3(j, n, s, [c | r])
  end
  defp binary_to_jid3(<<>>, n, s, r) do
    make(to_string(n), to_string(s), to_string(Enum.reverse(r)))
  end

  @spec to_binary(t) :: literal_jid
  def to_binary(jid(luser: user, lserver: server, lresource: resource)) do
    prefix = case user do
      "" -> ""
      _ -> user <> "@"
    end
    suffix = case resource do
      "" -> ""
      _ -> "/" <> resource
    end
    prefix <> server <> suffix
  end

  @spec nodeprep(server) :: {:ok, lserver} | :error
  defp nodeprep(s) when is_binary(s) and byte_size(s) < @sane_limit do
    s |> :stringprep.nodeprep |> prep_check()
  end
  defp nodeprep(_), do: :error

  @spec nameprep(server) :: {:ok, luser} | :error
  defp nameprep(s) when is_binary(s) and byte_size(s) < @sane_limit do
    s |> :stringprep.nameprep |> prep_check()
  end
  defp nameprep(_), do: :error

  @spec resourceprep(resource) :: {:ok, lresource} | :error
  defp resourceprep(s) when is_binary(s) and byte_size(s) < @sane_limit do
    s |> :stringprep.resourceprep |> prep_check()
  end
  defp resourceprep(_), do: :error

  defp prep_check(r) when byte_size(r) < @sane_limit, do: {:ok, r}
  defp prep_check(_), do: :error
end
