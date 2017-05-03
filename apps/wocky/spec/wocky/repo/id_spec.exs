defmodule Wocky.Repo.IDSpec do
  use ESpec, async: true

  alias Wocky.Repo.ID

  @uuid_str "fee55106-14a3-11e7-8166-4706bac47168"
  @uuid_bin <<254,229,81,6,20,163,17,231,129,102,71,6,186,196,113,104>>

  describe "new/0" do
    subject do: ID.new
    it do: should be_binary()
    it do: should be_valid_string()
    it do: should be_printable()
    it do: should have_length(36)
  end

  describe "to_string!/1" do
    it "should convert a binary UUID to a string" do
      @uuid_bin |> ID.to_string! |> should(eq @uuid_str)
    end

    it "should return a string UUID intact" do
      @uuid_str |> ID.to_string! |> should(eq @uuid_str)
    end

    it "should raise an error if the UUID is not valid" do
      # <<1,2,3>> |> ID.to_string! |> should(raise_exception ArgumentError)
      try do
        ID.to_string!(<<1,2,3>>)
        false
      rescue
        ArgumentError -> true
      end
      |> should(be_true())
    end

    it "should be transitive with to_binary!/1" do
      @uuid_bin |> ID.to_string! |> ID.to_binary! |> should(eq @uuid_bin)
    end
  end

  describe "to_string/1" do
    it "should return an :ok tuple on success" do
      @uuid_bin |> ID.to_string |> should(eq {:ok, @uuid_str})
    end

    it "should return an :error tuple on failure" do
      "alice" |> ID.to_string |> should(eq {:error, :invalid})
    end
  end

  describe "to_binary!/1" do
    it "should convert a string UUID to a binary" do
      @uuid_str |> ID.to_binary! |> should(eq @uuid_bin)
    end

    it "should return a binary UUID intact" do
      @uuid_bin |> ID.to_binary! |> should(eq @uuid_bin)
    end

    it "should raise an error if the UUID is not valid" do
      fn -> ID.to_binary!("alice") end
      |> should(raise_exception ArgumentError)
    end

    it "should be transitive with to_string!/1" do
      @uuid_str |> ID.to_binary! |> ID.to_string! |> should(eq @uuid_str)
    end
  end

  describe "to_binary/1" do
    it "should return an :ok tuple on success" do
      @uuid_str |> ID.to_binary |> should(eq {:ok, @uuid_bin})
    end

    it "should return an :error tuple on failure" do
      "alice" |> ID.to_binary |> should(eq {:error, :invalid})
    end
  end

  describe "valid?/1" do
    it "returns true if the user ID is valid" do
      [ID.new(:v1), :v1 |> ID.new |> ID.to_binary!,
       ID.new(:v4), :v4 |> ID.new |> ID.to_binary!]
      |> Enum.each(fn id -> id |> ID.valid? |> should(be_true()) end)
    end

    it "returns false if the user ID is not valid" do
      "alice" |> ID.valid? |> should(be_false())
    end
  end
end
