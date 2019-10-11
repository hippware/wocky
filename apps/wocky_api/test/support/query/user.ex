defmodule WockyAPI.Test.Query.User do
  @moduledoc false

  def user_delete do
    "mutation { userDelete { result } }"
  end
end
