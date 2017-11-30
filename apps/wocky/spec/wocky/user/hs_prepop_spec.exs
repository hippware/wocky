defmodule Wocky.User.HSPrepopSpec do
  use ESpec, async: true
  use ModelHelpers

  alias Wocky.InitialContact
  alias Wocky.Repo
  alias Wocky.RosterItem
  alias Wocky.User.HSPrepop

  before do
    # This user is inserted by the db migrations, however we'd rather have this
    # test also work properly on a completely empty DB
    user = Factory.build(:user, handle: HSPrepop.handle)
    Repo.insert(user, on_conflict: :nothing)
  end

  describe "add_source/1" do
    before do
      user = Factory.insert(:user)
      result = HSPrepop.add_source(user.handle)
      {:ok, user: user, result: result}
    end

    it "should return ok" do
      shared.result |> should(eq :ok)
    end

    it "should make the prepop user follow the specified user" do
      RosterItem.relationship(
        HSPrepop.user().id, shared.user.id)
      |> should(eq :follower)
    end

    it "should make the user an initial contact followee" do
      ic = InitialContact.get |> hd
      ic |> should(have user_id: shared.user.id)
      ic |> should(have type: :followee)
    end

  end

end
