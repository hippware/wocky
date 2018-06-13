# credo:disable-for-this-file Credo.Check.Refactor.PipeChainStart
defmodule Wocky.JIDSpec do
  use ESpec, async: true
  use Wocky.JID

  subject do: JID.make("a", "b", "c")

  describe "make/3" do
    it do: should(be_tuple())
    it do: JID.make(<<0>>, "b", "c") |> should(eq :error)
    it do: JID.make("a", "b", <<0>>) |> should(eq :error)
  end

  describe "equal?/2" do
    it do: assert(JID.equal?(subject(), subject()))
    it do: refute(JID.equal?(subject(), JID.make("a", "c")))
  end

  describe "from_binary/1" do
    it do: JID.from_binary("b") |> should(eq JID.make("b", ""))
    it do: JID.from_binary("a@b") |> should(eq JID.make("a", "b"))
    it do: JID.from_binary("a@b/c") |> should(eq JID.make("a", "b", "c"))
  end

  describe "to_binary/1" do
    it do: JID.to_binary(subject()) |> should(eq "a@b/c")
  end

  describe "to_bare/1" do
    it do: JID.to_bare(subject()) |> should(eq JID.make("a", "b", ""))
  end

  describe "replace_resource/2" do
    it do
      JID.replace_resource(subject(), "new_resource")
      |> should(eq JID.make("a", "b", "new_resource"))
    end
  end
end
