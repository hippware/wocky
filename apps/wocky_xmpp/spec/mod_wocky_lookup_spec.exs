defmodule :mod_wocky_lookup_spec do
  use ESpec, async: true, sandbox: true
  use SandboxHelper
  use IQHandlerSpec

  import :mod_wocky_lookup, only: [handle_handle_iq: 3, handle_phone_iq: 3]

  alias Wocky.Repo.Factory
  alias Wocky.User

  def make_iq(items) do
    iq_get(for i <- items, do: item_el(i))
  end

  def iq_get(items) do
    iq(
      type: :get,
      sub_el: xmlel(name: "lookup", children: items)
    )
  end

  def item_el(i) do
    xmlel(name: "item", attrs: [{"id", i}])
  end

  def result_iq(content) do
    iq(
      type: :result,
      sub_el: [
        xmlel(
          name: "results",
          attrs: [{"xmlns", "hippware.com/hxep/phone"}],
          children: content
        )
      ]
    )
  end

  before do
    user = Factory.insert(:user)

    {:ok,
     [
       user: user,
       phone: user.phone_number,
       handle: user.handle,
       user_jid: User.to_jid(user)
     ]}
  end

  describe "mod_wocky_lookup" do
    describe "handling an IQ 'get' for a phone number" do
      context "when there are no item elements in the IQ" do
        before do
          {:ok,
           [
             result: handle_phone_iq(shared.user_jid, @server_jid, make_iq([]))
           ]}
        end

        it "should return an empty result IQ" do
          shared.result |> should(eq result_iq([]))
        end
      end

      context "where there are items without an ID in the IQ" do
        before do
          result =
            handle_phone_iq(
              shared.user_jid,
              @server_jid,
              iq_get([xmlel(name: "item")])
            )

          {:ok, result: result}
        end

        it "should ignore them" do
          shared.result |> should(eq result_iq([]))
        end
      end

      context "when the IQ is properly formatted" do
        before do
          numbers = [shared.phone, "4567", "+5555", "+6666", "+9999"]

          result =
            handle_phone_iq(
              shared.user_jid,
              @server_jid,
              make_iq(numbers)
            )

          iq(sub_el: [xmlel(children: els)]) = result

          {:ok,
           [
             result: result,
             els: els,
             first: elem(List.first(els), 2),
             last: elem(List.last(els), 2)
           ]}
        end

        it "should return a result IQ" do
          iq(type: type) = shared.result
          type |> should(eq :result)
        end

        it "should return a result item for each item in the lookup list" do
          shared.els |> should(have_count 5)
        end

        it "should return item-not-found for an unrecognized number" do
          shared.last |> should(have {"error", "item-not-found"})
        end

        it "should return user information for a recognized number" do
          shared.first |> should(have {"id", shared.phone})
          shared.first |> should(have {"jid", JID.to_binary(shared.user_jid)})
          shared.first |> should(have {"handle", shared.handle})
        end
      end
    end

    describe "handling an IQ 'set' for a phone number" do
      before do
        result = handle_phone_iq(shared.user_jid, @server_jid, iq(type: :set))
        {:ok, result: result}
      end

      it "should return an error IQ" do
        iq(type: type) = shared.result
        type |> should(eq :error)
      end
    end

    describe "handling an IQ 'get' for a handle" do
      context "when there are no item elements in the IQ" do
        before do
          result = handle_handle_iq(shared.user_jid, @server_jid, make_iq([]))
          {:ok, result: result}
        end

        it "should return an empty result IQ" do
          shared.result |> should(eq result_iq([]))
        end
      end

      context "where there are items without an ID in the IQ" do
        before do
          result =
            handle_handle_iq(
              shared.user_jid,
              @server_jid,
              iq_get([xmlel(name: "item")])
            )

          {:ok, result: result}
        end

        it "should ignore them" do
          shared.result |> should(eq result_iq([]))
        end
      end

      context "when the request is properly formatted" do
        before do
          handles = [shared.handle, "4567", "+5555", "+6666", "+9999"]

          result =
            handle_handle_iq(
              shared.user_jid,
              @server_jid,
              make_iq(handles)
            )

          iq(sub_el: [xmlel(children: els)]) = result

          {:ok,
           [
             result: result,
             els: els,
             first: elem(List.first(els), 2),
             last: elem(List.last(els), 2)
           ]}
        end

        it "should return a result IQ" do
          iq(type: type) = shared.result
          type |> should(eq :result)
        end

        it "should return a result item for each item in the lookup list" do
          shared.els |> should(have_count 5)
        end

        it "should return item-not-found for an unrecognized handle" do
          shared.last |> should(have {"error", "item-not-found"})
        end

        it "should return user information for a recognized handle" do
          shared.first |> should(have {"id", shared.handle})
          shared.first |> should(have {"jid", JID.to_binary(shared.user_jid)})
        end
      end
    end

    describe "handling an IQ 'set' for a handle" do
      before do
        result = handle_handle_iq(shared.user_jid, @server_jid, iq(type: :set))
        {:ok, result: result}
      end

      it "should return an error IQ" do
        iq(type: type) = shared.result
        type |> should(eq :error)
      end
    end
  end
end
