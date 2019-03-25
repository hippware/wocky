defmodule Wocky.Push.Backend do
  @moduledoc """
  Backend behaviour for wocky push system
  """

  alias Wocky.Push

  @doc """
  Send a push message using the supplied parameters
  """
  @callback push(Push.t()) :: :ok

  @doc """
  Extract the id from the backend-specific notification
  """
  @callback get_id(Push.notification()) :: Push.id()

  @doc """
  Extract the payload from the backend-specific notification
  """
  @callback get_payload(Push.notification()) :: Push.payload()

  @doc """
  Extract the response from the backend-specific notification
  """
  @callback get_response(Push.notification()) :: Push.response()

  @doc """
  Handle a backend-specific error sending a push
  """
  @callback handle_error(Push.response()) :: :invalidate_token | :retry

  @doc """
  Produce a pretty string representation of a backend-specific error
  """
  @callback error_msg(Push.response()) :: binary()
end
