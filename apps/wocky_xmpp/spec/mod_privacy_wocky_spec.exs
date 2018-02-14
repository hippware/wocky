defmodule :mod_privacy_wocky_spec do
  use ESpec, async: true, sandbox: true
  use SandboxHelper
  use Wocky.JID

  import Record, only: [defrecordp: 2, extract: 2]
  import :mod_privacy_wocky

  alias Wocky.Repo.ID

  defrecordp :listitem,
             extract(:listitem, from_lib: "mongooseim/include/mod_privacy.hrl")

  @backend :mod_privacy_odbc
  @default_list "default"
  @privacy_list1 "privacy list 1"
  @privacy_list2 "privacy list 2"

  before do
    carol = ID.new()
    karen = ID.new()

    @backend.replace_privacy_list(carol, shared.server, @privacy_list1, [
      listitem(
        type: :jid,
        value: :jid.to_lower(JID.make(karen, shared.server)),
        action: :block,
        order: 1,
        match_all: true
      ),
      listitem(
        type: :jid,
        value: :jid.to_lower(JID.make(karen, shared.server)),
        action: :block,
        order: 2,
        match_iq: true
      )
    ])

    @backend.replace_privacy_list(carol, shared.server, @privacy_list2, [
      listitem(
        type: :subscription,
        value: :both,
        action: :block,
        order: 1,
        match_message: true
      )
    ])

    @backend.remove_user(karen, shared.server)

    {:ok, carol: carol, karen: karen}
  end

  describe "get_default_list/2" do
    before do
      {:ok,
       [
         default_items: default_list_items(shared.carol, shared.server),
         result: get_default_list(shared.carol, shared.server)
       ]}
    end

    it "should return a success result" do
      shared.result |> should(be_ok_result())
    end

    it "should contain the default list items" do
      {:ok, {@default_list, items}} = shared.result
      items |> should(eq shared.default_items)
    end
  end

  describe "get_list_names/2" do
    context "when the user has multiple lists" do
      before do
        {:ok, result: get_list_names(shared.carol, shared.server)}
      end

      it "should return a success result" do
        shared.result |> should(be_ok_result())
      end

      it "should return the user's lists" do
        {:ok, {_, lists}} = shared.result
        lists |> should(have_count 3)
        lists |> should(have @privacy_list1)
        lists |> should(have @privacy_list2)
      end

      it "should include the default list" do
        {:ok, {default, lists}} = shared.result
        default |> should(eq @default_list)
        lists |> should(have @default_list)
      end
    end

    context "when the user has no lists" do
      before do
        {:ok, result: get_list_names(shared.karen, shared.server)}
      end

      it "should return a success result" do
        shared.result |> should(be_ok_result())
      end

      it "should return the default list only" do
        {:ok, {default, lists}} = shared.result
        default |> should(eq @default_list)
        lists |> should(have_count 1)
        lists |> should(have @default_list)
      end
    end
  end

  describe "get_privacy_list/3" do
    context "when the named list exists" do
      before do
        result = get_privacy_list(shared.carol, shared.server, @privacy_list1)
        {:ok, result: result}
      end

      it "should return a success result" do
        shared.result |> should(be_ok_result())
      end

      it "should get the items in the list" do
        {:ok, items} = shared.result
        items |> should(have_length 2)
      end
    end

    context "when the named list does not exist" do
      before do
        result = get_privacy_list(shared.carol, shared.server, "not a list")
        {:ok, result: result}
      end

      it "should return an error result" do
        shared.result |> should(be_error_result())
      end

      it "should return a not_found error" do
        shared.result |> should(eq {:error, :not_found})
      end
    end

    context "when the user's list of privacy lists is empty" do
      before do
        result = get_privacy_list(shared.karen, shared.server, @privacy_list1)
        {:ok, result: result}
      end

      it "should return an error result" do
        shared.result |> should(be_error_result())
      end

      it "should return a not_found error" do
        shared.result |> should(eq {:error, :not_found})
      end
    end

    context "when the user does not exist" do
      before do
        result = get_privacy_list(ID.new(), shared.server, @privacy_list1)
        {:ok, result: result}
      end

      it "should return an error result" do
        shared.result |> should(be_error_result())
      end

      it "should return a not_found error" do
        shared.result |> should(eq {:error, :not_found})
      end
    end
  end

  describe "forget_default_list/2" do
    before do
      {:ok, result: forget_default_list(shared.carol, shared.server)}
    end

    it "should return an error result" do
      shared.result |> should(be_error_result())
    end

    it "should return a not_found error" do
      shared.result |> should(eq {:error, :not_found})
    end
  end

  describe "set_default_list/2" do
    before do
      {:ok,
       [
         result: set_default_list(shared.carol, shared.server, @privacy_list1)
       ]}
    end

    it "should return an error result" do
      shared.result |> should(be_error_result())
    end

    it "should return a not_found error" do
      shared.result |> should(eq {:error, :not_found})
    end
  end

  describe "remove_privacy_list/3" do
    context "when removing a user created list" do
      before do
        result =
          remove_privacy_list(shared.carol, shared.server, @privacy_list2)

        {:ok, result: result}
      end

      it "should return a success result" do
        shared.result |> should(eq :ok)
      end

      it "should remove the named list" do
        {:ok, {_, lists}} = get_list_names(shared.carol, shared.server)
        lists |> should(have_count 2)
        lists |> should_not(have @privacy_list2)
      end
    end

    context "when removing the default list" do
      before do
        result = remove_privacy_list(shared.karen, shared.server, @default_list)
        {:ok, result: result}
      end

      it "should return an error result" do
        shared.result |> should(be_error_result())
      end

      it "should return a conflict error" do
        shared.result |> should(eq {:error, :conflict})
      end

      it "should not remove the list" do
        {:ok, {_, lists}} = get_list_names(shared.karen, shared.server)
        lists |> should(have_count 1)
        lists |> should(have @default_list)
      end
    end
  end

  describe "replace_privacy_list/4" do
    context "when the list does not already exist" do
      before do
        result =
          replace_privacy_list(shared.karen, shared.server, "new list", [
            new_item()
          ])

        {:ok, result: result}
      end

      it "should return a success result" do
        shared.result |> should(eq :ok)
      end

      it "should create a new list" do
        get_privacy_list(shared.karen, shared.server, "new list")
        |> should(eq {:ok, [new_item()]})
      end
    end

    context "when the list already exists" do
      before do
        result =
          replace_privacy_list(shared.carol, shared.server, @privacy_list1, [
            new_item()
          ])

        {:ok, result: result}
      end

      it "should return a success result" do
        shared.result |> should(eq :ok)
      end

      it "should replace the list content" do
        get_privacy_list(shared.carol, shared.server, @privacy_list1)
        |> should(eq {:ok, [new_item()]})
      end
    end
  end

  describe "remove_user/2" do
    context "when the user exists" do
      before do
        {:ok, result: remove_user(shared.carol, shared.server)}
      end

      it "should return a success result" do
        shared.result |> should(eq :ok)
      end

      it "should remove the user's lists" do
        get_privacy_list(shared.carol, shared.server, @privacy_list1)
        |> should(eq {:error, :not_found})
      end

      it "should not fail with repeated calls" do
        remove_user(shared.carol, shared.server) |> should(eq :ok)
      end
    end

    context "when the user does not exist" do
      before do
        {:ok, result: remove_user(ID.new(), shared.server)}
      end

      it "should return a success result" do
        shared.result |> should(eq :ok)
      end
    end
  end

  defp new_item do
    listitem(
      type: :subscription,
      value: :both,
      action: :deny,
      order: 1,
      match_all: true
    )
  end
end
