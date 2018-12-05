defmodule WockyAPI.LocationControllerTest do
  use WockyAPI.ConnCase

  alias Wocky.Repo.Factory

  @create_attrs %{
    location: [
      %{
        coords: %{
          speed: -1,
          longitude: -85.7935821931526,
          latitude: 35.17448497921099,
          accuracy: 3000,
          heading: -1,
          altitude: 271.4361267089844,
          altitude_accuracy: 10
        },
        is_moving: true,
        odometer: 38_876_926.28380141,
        uuid: "3B30B2EA-4EF7-4B0F-927A-089921DA86FC",
        activity: %{
          type: "unknown",
          confidence: 100
        },
        battery: %{
          level: 0.78,
          is_charging: false
        },
        timestamp: "2016-10-24T09:45:05.621Z"
      }
    ],
    device: "testing"
  }

  setup %{conn: conn} do
    user = Factory.insert(:user, device: "testing")
    token = Factory.get_test_location_token(user)

    conn =
      conn
      |> put_req_header("accept", "application/json")
      |> put_req_header("authentication", "Bearer #{token}")

    {:ok, conn: conn, user: user}
  end

  describe "create location" do
    test "returns 201 when data is valid", %{conn: conn, user: user} do
      conn = post conn, location_path(conn, :create, user.id), @create_attrs
      assert response(conn, 201)
    end

    test "returns 201 when data is valid with fetch", %{conn: conn, user: user} do
      loc_with_fetch = @create_attrs.location |> hd |> Map.put(:isFetch, true)

      conn =
        post conn,
             location_path(conn, :create, user.id),
             Map.put(@create_attrs, :location, [loc_with_fetch])

      assert response(conn, 201)
    end

    test "returns 400 when latitude is missing", %{conn: conn, user: user} do
      invalid_attrs = %{
        location: %{
          coords: %{
            longitude: -85.7935821931526,
            accuracy: 3000
          }
        },
        device: "testing"
      }

      conn = post conn, location_path(conn, :create, user.id), invalid_attrs
      assert json_response(conn, 400)["errors"] != %{}
    end

    test "returns 400 when longitude is missing", %{conn: conn, user: user} do
      invalid_attrs = %{
        location: %{
          coords: %{
            latitude: 35.17448497921099,
            accuracy: 3000
          }
        },
        device: "testing"
      }

      conn = post conn, location_path(conn, :create, user.id), invalid_attrs
      assert json_response(conn, 400)["errors"] != %{}
    end

    test "returns 400 when accuracy is missing", %{conn: conn, user: user} do
      invalid_attrs = %{
        location: %{
          coords: %{
            longitude: -85.7935821931526,
            latitude: 35.17448497921099
          }
        },
        device: "testing"
      }

      conn = post conn, location_path(conn, :create, user.id), invalid_attrs
      assert json_response(conn, 400)["errors"] != %{}
    end

    test "returns 400 when device is missing", %{conn: conn, user: user} do
      invalid_attrs = %{
        location: %{
          coords: %{
            longitude: -85.7935821931526,
            latitude: 35.17448497921099,
            accuracy: 3000
          }
        }
      }

      conn = post conn, location_path(conn, :create, user.id), invalid_attrs
      assert json_response(conn, 400)["errors"] != %{}
    end
  end
end
