defmodule Wocky.IDSpec do
  use ESpec, async: true

  alias Wocky.ID

  describe "create/0" do
    subject do: ID.create
    it do: should be_binary()
    it do: should be_valid_string()
    it do: should be_printable()
    it do: should have_length(36)
  end

  describe "valid?/1" do
    it "returns true if the user ID is valid" do
      [:ossp_uuid.make(:v1, :text), :ossp_uuid.make(:v1, :binary),
       :ossp_uuid.make(:v4, :text), :ossp_uuid.make(:v4, :binary)]
      |> Enum.each(fn id -> id |> ID.valid? |> should(be_true()) end)
    end

    it "returns false if the user ID is not valid" do
      "alice" |> ID.valid? |> should(be_false())
    end
  end
end
