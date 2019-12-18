defmodule Wocky.ErrorsTest do
  use ExUnit.Case

  alias Wocky.Errors

  defmodule TestException do
    defexception [:foo]

    @impl true
    def message(_), do: "testing"
  end

  describe "error_to_string/1" do
    test "should work with an error map" do
      assert Errors.error_to_string(%{message: "testing"}) == "testing"
    end

    test "should work with an atom" do
      assert Errors.error_to_string(:testing) == "testing"
    end

    test "should work with a string" do
      assert Errors.error_to_string("testing") == "testing"
    end

    test "should work with an Exception" do
      assert Errors.error_to_string(%TestException{}) == "testing"
    end

    test "should have a reasonable fallback" do
      obj = ["testing"]

      assert Errors.error_to_string(obj) == inspect(obj)
    end
  end
end
