defmodule Wocky.User.HandleSpec do
  use ESpec, async: true

  alias Faker.Internet
  alias Golem.ID
  alias Wocky.User
  alias Wocky.User.Handle

  describe "check_reserved/1" do
    it "should return the handle when it is not reserved" do
      "notreserved" |> Handle.check_reserved |> should(eq {:ok, "notreserved"})
    end

    it "should return a :duplicate_handle error when the handle is reserved" do
      "root" |> Handle.check_reserved |> should(eq {:error, :duplicate_handle})
    end
  end

  describe "check_duplicate/1" do
    before do
      id = ID.new
      handle = Internet.user_name
      :ok = User.update(id, shared.server, %{handle: handle})
      :ok = User.wait_for_user(id)
      {:ok, id: id, handle: handle}
    end

    finally do
      User.delete(shared.server, shared.id)
    end

    it "should return the handle when it is unique" do
      "unique" |> Handle.check_duplicate |> should(eq {:ok, "unique"})
    end

    it "should return a :duplicate_handle error when handle is not unique" do
      shared.handle
      |> Handle.check_duplicate
      |> should(eq {:error, :duplicate_handle})
    end
  end
end
