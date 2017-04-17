defmodule Wocky.JID do
  @moduledoc "TODO"

  import Record, only: [defrecord: 2, is_record: 2]

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

  # A tuple-style JID
  @type simple_jid :: {user, server, resource}
  @type ljid :: {luser, lserver, lresource}

  # A tuple-style JID without resource part
  @type simple_bare_jid :: {luser, lserver}
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

  @spec make!(user, server, resource) :: t | no_return
  def make!(user, server, resource \\ "") do
    case make(user, server, resource) do
      :error -> raise ArgumentError
      jid -> jid
    end
  end

  @spec make(simple_jid) :: t | :error
  def make({user, server, resource}), do: make(user, server, resource)

  @spec make!(simple_jid) :: t | no_return
  def make!({user, server, resource}), do: make!(user, server, resource)

  @spec make_noprep(luser, lserver, lresource) :: t
  def make_noprep(luser, lserver, lresource) do
    jid(user: luser,
        server: lserver,
        resource: lresource,
        luser: luser,
        lserver: lserver,
        lresource: lresource)
  end

  @spec make_noprep(simple_jid) :: t
  def make_noprep({luser, lserver, lresource}),
    do: make_noprep(luser, lserver, lresource)

  @spec equal?(t, t) :: boolean
  def equal?(jid(luser: luser, lserver: lserver, lresource: lres),
             jid(luser: luser, lserver: lserver, lresource: lres)), do: true
  def equal?(_, _), do: false

  @doc "Returns true if `are_equal(to_bare(A), to_bare(B))`"
  @spec bare_equal?(t, t) :: boolean
  def bare_equal?(jid(luser: luser, lserver: lserver),
                  jid(luser: luser, lserver: lserver)), do: true
  def bare_equal?(_, _), do: false

  @spec from_binary(binary) :: t | :error
  def from_binary(j), do: binary_to_jid1(j, [])

  @spec from_binary!(binary) :: t | no_return
  def from_binary!(j) do
    case from_binary(j) do
      :error -> raise ArgumentError
      jid -> jid
    end
  end

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

  @spec to_binary(simple_jid | simple_bare_jid | jid) :: binary
  def to_binary(jid(user: user, server: server, resource: resource)) do
    to_binary({user, server, resource})
  end
  def to_binary({user, server}) do
    to_binary({user, server, ""})
  end
  def to_binary({user, server, resource}) do
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

  @spec nodename?(<<>> | binary) :: boolean
  def nodename?(""), do: false
  def nodename?(j), do: nodeprep(j) != :error

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

  @spec to_lower(simple_jid | jid) :: simple_jid | :error
  def to_lower(jid(luser: u, lserver: s, lresource: r)), do: {u, s, r}
  def to_lower({u, s, r}) do
    with {:ok, luser} <- nodeprep(u),
         {:ok, lserver} <- nameprep(s),
         {:ok, lresource} <- resourceprep(r)
    do
      {luser, lserver, lresource}
    else
      :error -> :error
    end
  end

  @spec to_lus(jid) :: simple_bare_jid
  def to_lus(jid(luser: u, lserver: s)), do: {u, s}

  @spec to_bare(simple_jid | jid) :: simple_jid | jid
  def to_bare({u, s, _r}), do: {u, s, ""}
  def to_bare(full_jid) when is_record(full_jid, :jid) do
    jid(full_jid, resource: "", lresource: "")
  end

  @spec replace_resource(jid, resource) :: jid | :error
  def replace_resource(jid, resource) do
    case resourceprep(resource) do
      :error -> :error
      {:ok, lresource} -> jid(jid, resource: resource, lresource: lresource)
    end
  end

  @spec binary_to_bare(binary) :: jid | :error
  def binary_to_bare(jid) when is_binary(jid) do
    case from_binary(jid) do
      :error -> :error
      result -> to_bare(result)
    end
  end
end
