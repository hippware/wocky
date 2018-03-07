defmodule Wocky.Account.Firebase do
  @moduledoc """
  Module for verifying Firebase JWT tokens
  """

  use GuardianFirebase,
    otp_app: :wocky,
    project_id: "my-project-1480497595993"

  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.User

  @provider "firebase"

  def subject_for_token(%User{} = user, _claims) do
    case user.external_id do
      nil -> assign_random_external_id(user)
      external_id -> {:ok, external_id}
    end
  end
  def subject_for_token(_, _) do
    {:error, :unknown_resource}
  end

  def resource_from_claims(%{sub: external_id, phone_number: phone_number}) do
    case Repo.get_by(User, external_id: external_id, provider: @provider) do
      nil ->
        case Repo.get_by(User, phone_number: phone_number) do
          nil ->
            {:error, :not_found}

          orig_user ->
            User.update(orig_user, %{
              provider: @provider,
              external_id: external_id,
              phone_number: phone_number
            })
        end

      user ->
        {:ok, user}
    end
  end
  def resource_from_claims(_claims) do
    {:error, :not_possible}
  end

  defp assign_random_external_id(user) do
    external_id = Factory.external_id()
    {:ok, _} =
      User.update(user, %{
        provider: @provider,
        external_id: external_id
      })

    {:ok, external_id}
  end
end
