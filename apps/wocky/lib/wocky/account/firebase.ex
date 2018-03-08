defmodule Wocky.Account.Firebase do
  @moduledoc """
  Module for verifying Firebase JWT tokens
  """

  use GuardianFirebase,
    otp_app: :wocky,
    project_id: "my-project-1480497595993"

  alias Wocky.Account.Register
  alias Wocky.User

  def subject_for_token(%User{} = user, _claims) do
    {:ok, Register.get_external_id(user)}
  end
  def subject_for_token(_, _) do
    {:error, :unknown_resource}
  end

  def resource_from_claims(%{sub: external_id, phone_number: phone_number}) do
    Register.find(:firebase, external_id, phone_number)
  end
  def resource_from_claims(_claims) do
    {:error, :not_possible}
  end
end
