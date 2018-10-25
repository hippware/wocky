defmodule Wocky.Repo.IDTest do
  use ExUnit.Case, async: true

  alias Wocky.Repo.ID

  @uuid_str "fee55106-14a3-11e7-8166-4706bac47168"
  @uuid_bin <<254, 229, 81, 6, 20, 163, 17, 231, 129, 102, 71, 6, 186, 196, 113,
              104>>

  test "new/0" do
    subject = ID.new()
    assert is_binary(subject)
    assert String.valid?(subject)
    assert String.printable?(subject)
    assert String.length(subject) == 36
  end

  describe "to_string!/1" do
    test "should convert a binary UUID to a string" do
      assert ID.to_string!(@uuid_bin) == @uuid_str
    end

    test "should return a string UUID intact" do
      assert ID.to_string!(@uuid_str) == @uuid_str
    end

    test "should raise an error if the UUID is not valid" do
      assert_raise ArgumentError, fn ->
        ID.to_string!(<<1, 2, 3>>)
      end
    end

    test "should be transitive with to_binary!/1" do
      assert ID.to_binary!(ID.to_string!(@uuid_bin)) == @uuid_bin
    end
  end

  describe "to_string/1" do
    test "should return an :ok tuple on success" do
      assert ID.to_string(@uuid_bin) == {:ok, @uuid_str}
    end

    test "should return an :error tuple on failure" do
      assert ID.to_string("alice") == {:error, :invalid}
    end
  end

  describe "to_binary!/1" do
    test "should convert a string UUID to a binary" do
      assert ID.to_binary!(@uuid_str) == @uuid_bin
    end

    test "should return a binary UUID intact" do
      assert ID.to_binary!(@uuid_bin) == @uuid_bin
    end

    test "should raise an error if the UUID is not valid" do
      assert_raise ArgumentError, fn ->
        ID.to_binary!("alice")
      end
    end

    test "should be transitive with to_string!/1" do
      assert ID.to_string!(ID.to_binary!(@uuid_str)) == @uuid_str
    end
  end

  describe "to_binary/1" do
    test "should return an :ok tuple on success" do
      assert ID.to_binary(@uuid_str) == {:ok, @uuid_bin}
    end

    test "should return an :error tuple on failure" do
      assert ID.to_binary("alice") == {:error, :invalid}
    end
  end

  describe "valid?/1" do
    test "returns true if the user ID is valid" do
      [
        ID.new(:v1),
        ID.new(:v1) |> ID.to_string!(),
        ID.new(:v4),
        ID.new(:v4) |> ID.to_string!()
      ]
      |> Enum.each(fn id -> assert ID.valid?(id) end)
    end

    test "returns false if the user ID is not valid" do
      refute ID.valid?("alice")
      refute ID.valid?("hello world url!")
    end
  end
end
