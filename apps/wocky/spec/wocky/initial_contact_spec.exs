defmodule Wocky.InitialContactSpec do
  use ESpec, async: true
  use ModelHelpers

  alias Wocky.InitialContact
  alias Wocky.Repo

  before do
    Repo.delete_all(InitialContact)

    init_contacts =
      5
      |> Factory.insert_list(:user)
      |> Enum.map(fn(c) ->
                    :initial_contact
                    |> Factory.insert(user_id: c.id)
                    |> Repo.preload(:user)
                  end)
      |> Enum.sort

    {:ok,
      init_contacts: init_contacts
    }
  end

  describe "get/0" do
    it "should return all initial contacts" do
      InitialContact.get |> Enum.sort |> should(eq shared.init_contacts)
    end
  end
end
