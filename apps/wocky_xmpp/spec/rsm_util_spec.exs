defmodule :rsm_util_spec do
  use ESpec, async: true
  use XMLHelper

  import :rsm_util, only: [get_rsm: 1, filter_with_rsm: 2]
  import Record, only: [defrecordp: 2, extract: 2]

  require Record

  defrecordp :rsm_in, extract(:rsm_in, from_lib: "mongooseim/include/jlib.hrl")
  defrecordp :rsm_out,
    extract(:rsm_out, from_lib: "mongooseim/include/jlib.hrl")

  @ns_rsm "http://jabber.org/protocol/rsm"
  @count 200

  describe "get_rsm/1" do
    context "when passed an IQ with a standard RSM" do
      before do
        iq = iq_with_rsm(5, :aft, "abc", :undefined, true)
        {:ok, result: get_rsm(iq)}
      end

      it "should return a success result" do
        shared.result |> should(be_ok_result())
      end

      it "should return the decoded RSM" do
        {:ok, rsm} = shared.result

        rsm
        |> should(
          eq rsm_in(
               max: 5,
               direction: :aft,
               id: "abc",
               index: :undefined,
               reverse: true
             )
        )
      end
    end

    context "when passed an IQ without an RSM" do
      before do
        iq =
          iq(
            id: "abc",
            type: "result",
            sub_el: xmlel(name: "blah")
          )

        {:ok, result: get_rsm(iq)}
      end

      it "should return an error result" do
        shared.result |> should(be_error_result())
      end
    end
  end

  describe "filter_with_rsm/2" do
    let :data, do: for(id <- 1..@count, do: %{id: id, val: :erlang.phash2(id)})

    let :unordered_data,
      do: for(id <- 1..@count, do: %{id: unordered_id(id), val: id})

    context "when no RSM values are set" do
      it "should return ordered data unchanged" do
        {data_out, rsm_out} = filter_with_rsm(data(), rsm_in())

        data_out |> should(eq data())

        rsm_out
        |> should(
          eq rsm_out(
               count: @count,
               index: 0,
               first: to_string(1),
               last: to_string(@count)
             )
        )
      end

      it "should return unordered data unchanged" do
        {data_out, rsm_out} = filter_with_rsm(unordered_data(), rsm_in())

        data_out |> should(eq unordered_data())

        rsm_out
        |> should(
          eq rsm_out(
               count: @count,
               index: 0,
               first: 1 |> unordered_id() |> to_string,
               last: @count |> unordered_id() |> to_string
             )
        )
      end

      it "should return an empty dataset unchanged" do
        {data_out, rsm_out} = filter_with_rsm([], rsm_in())

        data_out |> should(eq [])
        rsm_out |> should(eq rsm_out(count: 0))
      end
    end

    context "when a limit filter is present" do
      it "should return a subset of ordered data" do
        {data_out, rsm_out} = filter_with_rsm(data(), rsm_in(max: 50))

        data_out |> should(eq Enum.take(data(), 50))

        rsm_out
        |> should(
          eq rsm_out(
               count: @count,
               index: 0,
               first: to_string(1),
               last: to_string(50)
             )
        )
      end

      it "should return a subset of unordered data" do
        {data_out, rsm_out} = filter_with_rsm(unordered_data(), rsm_in(max: 50))

        data_out |> should(eq Enum.take(unordered_data(), 50))

        rsm_out
        |> should(
          eq rsm_out(
               count: @count,
               index: 0,
               first: 1 |> unordered_id() |> to_string,
               last: 50 |> unordered_id() |> to_string
             )
        )
      end

      it "should return an empty dataset unchanged" do
        {data_out, rsm_out} = filter_with_rsm([], rsm_in(max: 50))

        data_out |> should(eq [])
        rsm_out |> should(eq rsm_out(count: 0))
      end
    end

    context "when an ID filter is present" do
      it "should return ordered data from the specified ID" do
        {data_out, rsm_out} =
          filter_with_rsm(data(), rsm_in(max: 10, direction: :aft, id: 50))

        data_out |> should(eq Enum.slice(data(), 50, 10))

        rsm_out
        |> should(
          eq rsm_out(
               count: @count,
               index: 50,
               first: to_string(51),
               last: to_string(60)
             )
        )
      end

      it "should return unordered data from the specified ID" do
        {data_out, rsm_out} =
          filter_with_rsm(
            unordered_data(),
            rsm_in(max: 10, direction: :aft, id: unordered_id(50))
          )

        data_out |> should(eq Enum.slice(unordered_data(), 50, 10))

        rsm_out
        |> should(
          eq rsm_out(
               count: @count,
               index: 50,
               first: 51 |> unordered_id() |> to_string,
               last: 60 |> unordered_id() |> to_string
             )
        )
      end

      it "should return an empty dataset unchanged" do
        {data_out, rsm_out} =
          filter_with_rsm([], rsm_in(max: 10, direction: :aft, id: 50))

        data_out |> should(eq [])
        rsm_out |> should(eq rsm_out(count: 0))
      end

      it "should return an empty result set for an invalid id" do
        {data_out, rsm_out} =
          filter_with_rsm(data(), rsm_in(max: 10, direction: :aft, id: -100))

        data_out |> should(eq [])
        rsm_out |> should(eq rsm_out(count: @count))
      end
    end

    context "when an index filter is present" do
      it "should return ordered data from the specified index" do
        {data_out, rsm_out} =
          filter_with_rsm(data(), rsm_in(max: 10, direction: :aft, index: 50))

        data_out |> should(eq Enum.slice(data(), 50, 10))

        rsm_out
        |> should(
          eq rsm_out(
               count: @count,
               index: 50,
               first: to_string(51),
               last: to_string(60)
             )
        )
      end

      it "should return unordered data from the specified index" do
        {data_out, rsm_out} =
          filter_with_rsm(
            unordered_data(),
            rsm_in(max: 10, direction: :aft, index: 50)
          )

        data_out |> should(eq Enum.slice(unordered_data(), 50, 10))

        rsm_out
        |> should(
          eq rsm_out(
               count: @count,
               index: 50,
               first: 51 |> unordered_id() |> to_string,
               last: 60 |> unordered_id() |> to_string
             )
        )
      end

      it "should return an empty dataset unchanged" do
        {data_out, rsm_out} =
          filter_with_rsm([], rsm_in(max: 10, direction: :aft, index: 50))

        data_out |> should(eq [])
        rsm_out |> should(eq rsm_out(count: 0))
      end

      it "should return an empty result set for an invalid index" do
        {data_out, rsm_out} =
          filter_with_rsm(
            data(),
            rsm_in(max: 10, direction: :aft, index: @count + 50)
          )

        data_out |> should(eq [])
        rsm_out |> should(eq rsm_out(count: @count))
      end
    end

    context "when a before filter is present" do
      it "should get items at the end of a list when there is no id or index" do
        {data_out, rsm_out} =
          filter_with_rsm(data(), rsm_in(max: 10, direction: :before))

        data_out |> should(eq Enum.slice(data(), @count - 10, 10))

        rsm_out
        |> should(
          eq rsm_out(
               count: @count,
               index: @count - 10,
               first: to_string(@count - 9),
               last: to_string(@count)
             )
        )
      end

      it "should get items before a specified id" do
        {data_out, rsm_out} =
          filter_with_rsm(
            data(),
            rsm_in(max: 10, direction: :before, id: @count - 49)
          )

        data_out |> should(eq Enum.slice(data(), @count - 60, 10))

        rsm_out
        |> should(
          eq rsm_out(
               count: @count,
               index: @count - 60,
               first: to_string(@count - 59),
               last: to_string(@count - 50)
             )
        )
      end

      it "should return an empty dataset unchanged" do
        {data_out, rsm_out} =
          filter_with_rsm([], rsm_in(max: 10, direction: :before))

        data_out |> should(eq [])
        rsm_out |> should(eq rsm_out(count: 0))
      end

      it "should return an empty dataset for an invalid 'before' id" do
        {data_out, rsm_out} =
          filter_with_rsm(
            data(),
            rsm_in(max: 10, direction: :aft, id: @count + 50)
          )

        data_out |> should(eq [])
        rsm_out |> should(eq rsm_out(count: @count))
      end
    end

    context "when a reverse filter is present" do
      it "should reverse the results" do
        {data1, rsm1} =
          filter_with_rsm(data(), rsm_in(max: 10, direction: :before))

        {data2, rsm2} =
          filter_with_rsm(
            data(),
            rsm_in(max: 10, direction: :before, reverse: true)
          )

        data1 |> should(eq Enum.reverse(data2))
        rsm1 |> should(eq rsm2)
      end
    end
  end

  defp unordered_id(id), do: :erlang.phash2(id)

  defp iq_with_rsm(max, dir, id, index, reverse) do
    iq(
      id: "some id",
      type: "request",
      sub_el:
        xmlel(
          name: "operation",
          children:
            maybe_rand_tags() ++
              [rsm_tag(max, dir, id, index, reverse)] ++ maybe_rand_tags()
        )
    )
  end

  defp maybe_rand_tags do
    # Produce 0..2 random tags
    for _ <- 0..(:rand.uniform(3) - 1), do: rand_tag()
  end

  defp rand_tag do
    x = Integer.to_string(:rand.uniform(1000))
    xmlel(name: x, children: [xmlcdata(content: x)])
  end

  defp rsm_tag(max, dir, id, index, reverse) do
    xmlel(
      name: "set",
      attrs: [{"xmlns", @ns_rsm}],
      children: rsm_children(max, dir, id, index, reverse, [])
    )
  end

  # credo:disable-for-next-line Credo.Check.Refactor.FunctionArity
  defp rsm_children(:undefined, dir, id, index, reverse, acc) do
    rsm_children(dir, id, index, reverse, acc)
  end

  # credo:disable-for-next-line Credo.Check.Refactor.FunctionArity
  defp rsm_children(max, dir, id, index, reverse, acc) do
    rsm_children(dir, id, index, reverse, [
      xml_with_cdata("max", Integer.to_string(max)) | acc
    ])
  end

  defp rsm_children(:undefined, _id, index, reverse, acc) do
    rsm_children(index, reverse, acc)
  end

  defp rsm_children(:before, id, index, reverse, acc) do
    rsm_children(index, reverse, [dir_element("before", id) | acc])
  end

  defp rsm_children(:aft, id, index, reverse, acc) do
    rsm_children(index, reverse, [dir_element("after", id) | acc])
  end

  defp rsm_children(:undefined, reverse, acc) do
    rsm_children(reverse, acc)
  end

  defp rsm_children(index, reverse, acc) do
    rsm_children(reverse, [
      xml_with_cdata("index", Integer.to_string(index)) | acc
    ])
  end

  defp rsm_children(false, acc), do: acc
  defp rsm_children(true, acc), do: [xmlel(name: "reverse") | acc]

  defp dir_element(name, :undefined), do: xmlel(name: name)
  defp dir_element(name, id), do: xml_with_cdata(name, id)

  defp xml_with_cdata(name, cdata),
    do: xmlel(name: name, children: [xmlcdata(content: cdata)])
end
