defmodule WockyAPI.PipelineLogTest do
  use Wocky.DataCase

  import WockyAPI.PipelineLog

  alias Absinthe.Blueprint
  alias Faker.Lorem
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.TrafficLog

  setup do
    user = Factory.insert(:user)

    {:ok,
     opts: %{
       context: %{
         current_user: user,
         host: Lorem.word(),
         peer: Lorem.word()
       }
     }}
  end

  describe "run/2" do
    test "should log incoming requests", %{opts: opts} do
      packet = Lorem.sentence()
      run(packet, Map.put(opts, :phase, :request))

      match(opts, false, packet <> " / " <> inspect(nil))
    end

    test "should log outgoing requests", %{opts: opts} do
      blueprint = %Blueprint{
        result: %{
          data: %{some: "map"},
          errors: %{other: "map"}
        }
      }

      run(blueprint, Map.put(opts, :phase, :response))

      match(opts, true, "%{some: \"map\"} / %{other: \"map\"}")
    end

    test "should work with only error output", %{opts: opts} do
      blueprint = %Blueprint{
        result: %{
          errors: %{other: "map"}
        }
      }

      run(blueprint, Map.put(opts, :phase, :response))
      match(opts, true, "nil / %{other: \"map\"}")
    end
  end

  defp match(opts, incoming, packet) do
    host = opts.context.host
    peer = opts.context.peer
    user_id = opts.context.current_user.id

    assert %{
             resource: "GraphQL",
             host: ^host,
             ip: ^peer,
             incoming: ^incoming,
             packet: ^packet,
             user_id: ^user_id
           } = get_traffic(opts.context.current_user)
  end

  defp get_traffic(user) do
    TrafficLog
    |> where([t], t.user_id == ^user.id)
    |> Repo.one()
  end
end
