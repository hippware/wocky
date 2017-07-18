defmodule Wocky.InitialFolloweeSpec do
  use ESpec, async: true
  use ModelHelpers

  alias Wocky.InitialFollowee
  alias Wocky.Repo

  before do
    Repo.delete_all(InitialFollowee)
    followees = Factory.insert_list(4, :user)
    Enum.each(followees, &Factory.insert(:initial_followee, user_id: &1.id))

    {:ok,
      followees: Enum.sort(followees)
    }
  end

  describe "get/0" do
    it "should return all initial followees" do
      InitialFollowee.get |> Enum.sort |> should(eq shared.followees)
    end
  end
end
