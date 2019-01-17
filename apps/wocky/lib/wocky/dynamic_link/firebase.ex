defmodule Wocky.DynamicLink.Firebase do
  @moduledoc """
  Module for generating dynamic links using firebase
  """

  @behaviour Wocky.DynamicLink

  alias FirebaseAdminEx.DynamicLink, as: FirebaseDynamicLink

  def invitation_link(invitation_code) do
    params = %{
      "domainUriPrefix" => Confex.get_env(:wocky, :firebase_domain_url_prefix),
      "iosInfo" => %{
        "iosAppStoreId" => Confex.get_env(:wocky, :app_store_id),
        "iosBundleId" => Confex.get_env(:wocky, :ios_bundle_id)
      },
      "link" => Confex.get_env(:wocky, :firebase_link_prefix) <> invitation_code
    }

    with {:ok, result} <- FirebaseDynamicLink.short_link(params) do
      link = result["shortLink"]

      if link != nil do
        {:ok, link}
      else
        {:error, "Link generation failed. Received: #{inspect(result)}"}
      end
    end
  end
end
