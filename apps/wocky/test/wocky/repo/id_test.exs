defmodule Wocky.Repo.IDTest do
  use ExUnit.Case, async: true

  alias Wocky.Repo.ID

  test "new/0" do
    subject = ID.new()
    assert is_binary(subject)
    assert String.valid?(subject)
    assert String.printable?(subject)
    assert String.length(subject) == 36
  end

  describe "valid?/1" do
    test "returns true if the user ID is valid" do
      assert ID.valid?(ID.new())
    end

    test "returns false if the user ID is not valid" do
      refute ID.valid?("alice")
      refute ID.valid?("hello world url!")
    end
  end
end
