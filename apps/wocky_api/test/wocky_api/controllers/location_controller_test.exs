defmodule WockyAPI.Controllers.LocationControllerTest do
  use WockyAPI.ConnCase

  alias Faker.Address
  alias Wocky.Location
  alias Wocky.Repo.Factory
  alias WockyAPI.Factory, as: APIFactory

  @location %{
    coords: %{
      speed: -1,
      longitude: -85.7935821931526,
      latitude: 35.17448497921099,
      accuracy: 30,
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
    timestamp: "2016-10-24T09:45:05.621Z",
    extra_field: "extra_data",
    extra_field_2: "extra_data_2"
  }

  defp packet(loc \\ @location, device \\ "testing"),
    do: %{location: loc, device: device}

  defp random_location do
    coords =
      @location.coords
      |> Map.put(:latitude, Address.latitude())
      |> Map.put(:longitude, Address.longitude())

    Map.put(@location, :coords, coords)
  end

  setup %{conn: conn} do
    user = Factory.insert(:user, device: "testing")
    token = APIFactory.get_test_location_token(user)

    conn =
      conn
      |> put_req_header("accept", "application/json")
      |> put_req_header("authentication", "Bearer #{token}")

    {:ok, conn: conn, user: user}
  end

  describe "create location" do
    test "returns 201 and watched status when data is valid", %{
      conn: conn,
      user: user
    } do
      Location.inc_watcher_count(user)
      conn = post conn, location_path(conn, :create, user.id), packet()
      assert response(conn, 201) =~ ~s/"watched":true/
      assert response(conn, 201) =~ ~s/"watchers":1/
    end

    test "caches location when data is valid", %{conn: conn, user: user} do
      post conn, location_path(conn, :create, user.id), packet()

      cur_loc = Location.get_current_user_location(user)
      assert cur_loc
      assert cur_loc.lat == @location.coords.latitude
      assert cur_loc.lon == @location.coords.longitude
    end

    test "returns 201 when data is valid with fetch", %{conn: conn, user: user} do
      loc_with_fetch = @location |> Map.put(:isFetch, true)

      conn =
        post conn,
             location_path(conn, :create, user.id),
             packet([loc_with_fetch])

      assert response(conn, 201)
    end

    test "returns 201 without battery or activity data", %{
      conn: conn,
      user: user
    } do
      location = Map.drop(@location, [:battery, :activity])

      conn =
        post conn, location_path(conn, :create, user.id), packet([location])

      assert response(conn, 201)
    end

    test "processes all data points in a batch", %{conn: conn, user: user} do
      current = random_location()
      locations = [random_location(), random_location(), current]
      conn = post conn, location_path(conn, :create, user.id), packet(locations)
      assert response(conn, 201)

      cur_loc = Location.get_current_user_location(user)
      assert cur_loc
      assert cur_loc.lat == current.coords.latitude
      assert cur_loc.lon == current.coords.longitude
    end

    test "silently fails when latitude is missing", %{conn: conn, user: user} do
      invalid_attrs =
        packet([
          %{
            coords: %{
              longitude: -85.7935821931526,
              accuracy: 3000
            }
          }
        ])

      post conn, location_path(conn, :create, user.id), invalid_attrs

      refute Location.get_current_user_location(user)
    end

    test "silently fails when longitude is missing", %{conn: conn, user: user} do
      invalid_attrs =
        packet([
          %{
            coords: %{
              latitude: 35.17448497921099,
              accuracy: 3000
            }
          }
        ])

      post conn, location_path(conn, :create, user.id), invalid_attrs

      refute Location.get_current_user_location(user)
    end

    test "silently fails when accuracy is missing", %{conn: conn, user: user} do
      invalid_attrs =
        packet([
          %{
            coords: %{
              longitude: -85.7935821931526,
              latitude: 35.17448497921099
            }
          }
        ])

      post conn, location_path(conn, :create, user.id), invalid_attrs

      refute Location.get_current_user_location(user)
    end

    test "returns 400 when device is missing", %{conn: conn, user: user} do
      invalid_attrs = packet([@location], nil)
      conn = post conn, location_path(conn, :create, user.id), invalid_attrs
      assert conn.status == 400
    end

    test "assigns extra fields", %{conn: conn, user: user} do
      post conn, location_path(conn, :create, user.id), packet()

      cur_loc = Location.get_current_user_location(user)
      assert cur_loc.extra_fields["extra_field"] == "extra_data"
      assert cur_loc.extra_fields["extra_field_2"] == "extra_data_2"
      assert Enum.count(cur_loc.extra_fields) == 2
    end
  end
end
