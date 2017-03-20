defmodule Wocky.Repo.DocSpec do
  use ESpec, async: true

  require Record
  alias Wocky.Repo.Doc

  describe "new/1" do
    # subject(Doc.new(%{a: 1, b: 2})

    it "should return a map record" do
      assert {:map, _, _, _, _} = Doc.new(%{a: 1, b: 2})
    end
  end

  describe "to_map/1" do

  end
end
