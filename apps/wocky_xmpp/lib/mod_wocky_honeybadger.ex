defmodule :mod_wocky_honeybadger do
  @moduledoc "Reports IQ handler crashes to the Honeybadger service"

  import Record, only: [defrecordp: 2, extract: 2]

  require Honeybadger
  require Logger

  @behaviour :gen_mod

  @hook_priority 5

  defrecordp :iq, extract(:iq, from_lib: "ejabberd/include/jlib.hrl")
  defrecordp :jid, extract(:jid, from_lib: "ejabberd/include/jlib.hrl")

  # ===================================================================
  # gen_mod handlers
  # ===================================================================

  def start(host, _opts) do
    :ejabberd_hooks.add(:sm_register_connection_hook, host,
                        &sm_register_connection_hook/3, @hook_priority)

    :ejabberd_hooks.add(:filter_local_packet, host,
                        &filter_local_packet_hook/1, @hook_priority)

    :ejabberd_hooks.add(:iq_handler_crash, host,
                        &iq_handler_crash_hook/4, @hook_priority)
  end

  def stop(host) do
    :ejabberd_hooks.delete(:iq_handler_crash, host,
                           &iq_handler_crash_hook/4, @hook_priority)

    :ejabberd_hooks.delete(:filter_local_packet, host,
                           &filter_local_packet_hook/1, @hook_priority)

    :ejabberd_hooks.delete(:sm_register_connection_hook, host,
                           &sm_register_connection_hook/3, @hook_priority)
  end

  # ===================================================================
  # User connection handler
  # ===================================================================

  @spec sm_register_connection_hook(:ejabberd_sm.sid, :ejabberd.jid, any) :: :ok
  def sm_register_connection_hook(sid, jid, info) do
    Honeybadger.context(%{user_id: jid(jid, :luser),
                          connection: %{JID: :jid.to_binary(jid),
                                        SID: inspect(sid),
                                        info: inspect(info)}})
    :ok
  end

  # ===================================================================
  # Incoming packet handler
  # ===================================================================

  @type filter_packet() :: {:ejabberd.jid, :ejabberd.jid, :jlib.xmlel}
  @spec filter_local_packet_hook(filter_packet | :drop) :: filter_packet
  def filter_local_packet_hook({from, to, packet} = p) do
    Honeybadger.context(%{last_packet: %{from: :jid.to_binary(from),
                                         to: :jid.to_binary(to),
                                         packet: :exml.to_binary(packet)}})
    p
  end
  def filter_local_packet_hook(:drop), do: :drop

  # ===================================================================
  # IQ handler crash
  # ===================================================================

  @spec iq_handler_crash_hook(:ejabberd.jid, :ejabberd.jid, :ejabberd.iq, any)
  :: :ok
  def iq_handler_crash_hook(from, to, iq, exception) do
    stacktrace = :erlang.get_stacktrace
    Honeybadger.notify(
      "IQ handler crash: #{iq(iq, :xmlns)}",
      %{user_id: jid(from, :luser),
        exception: inspect(exception),
        iq: %{
          from: :jid.to_binary(from),
          to: :jid.to_binary(to),
          id: iq(iq, :id),
          type: to_string(iq(iq, :type)),
          ns: iq(iq, :xmlns),
          payload: :exml.to_binary(iq(iq, :sub_el))
        }
      },
      stacktrace
    )

    :ok
  end
end
