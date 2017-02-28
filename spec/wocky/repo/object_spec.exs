defmodule Wocky.Repo.ObjectSpec do
  use ESpec, async: true

  require Record
  alias Wocky.Repo.Object

  describe "new/1" do
    # subject(Object.new(%{a: 1, b: 2})

    it "should return a map record" do
      assert {:map, _, _, _, _} = Object.new(%{a: 1, b: 2})
    end
  end

  describe "to_map/1" do

  end
end
