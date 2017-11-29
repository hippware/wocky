defmodule Wocky.User.SpecialSpec do
  use ESpec, async: true
  use ModelHelpers

  alias Wocky.InitialContact
  alias Wocky.RosterItem
  alias Wocky.User.Special

  before do
    Factory.insert(:user, handle: Special.hs_prepopulation_handle())
  end

  describe "add_hs_prepop_source/1" do
    before do
      user = Factory.insert(:user)
      result = Special.add_hs_prepop_source(user.handle)
      {:ok, user: user, result: result}
    end

    it "should return ok" do
      shared.result |> should(eq :ok)
    end

    it "should make the prepop user follow the specified user" do
      RosterItem.relationship(
        Special.hs_prepopulation_user().id, shared.user.id)
      |> should(eq :follower)
    end

    it "should make the user an initial contact followee" do
      ic = InitialContact.get |> hd
      ic |> should(have user_id: shared.user.id)
      ic |> should(have type: :followee)
    end

  end

end
