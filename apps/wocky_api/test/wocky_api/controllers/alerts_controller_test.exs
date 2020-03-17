defmodule WockyAPI.Controllers.AlertsControllerTest do
  use WockyAPI.ConnCase

  import Ecto.Query, only: [from: 2]

  alias Faker.Code
  alias Faker.Lorem
  alias Wocky.Alerts.Alert
  alias Wocky.Repo

  setup %{conn: conn} do
    conn = put_req_header(conn, "content-type", "application/json")

    {:ok, conn: conn}
  end

  describe "create alert with embedded geometry" do
    setup %{conn: conn} do
      source_id = Code.isbn13()

      packet = %{
        "source" => "test",
        "source_id" => source_id,
        "title" => Lorem.sentence(),
        "summary" => Lorem.paragraph(),
        "geometry" => %{"type" => "Point", "coordinates" => [100.0, 0.0]}
      }

      path = alerts_path(conn, :create, "test", source_id)
      conn = put conn, path, packet

      {:ok, conn: conn, source_id: source_id}
    end

    test "returns 201", %{conn: conn} do
      assert response(conn, 201)
    end

    test "stores the alert record in the database", ctx do
      rec = Repo.get_by(Alert, source: "test", source_id: ctx.source_id)

      assert rec && rec.geometry.coordinates == {100.0, 0.0}
    end
  end

  describe "create alert with geometry lookup" do
    setup %{conn: conn} do
      geom1 = Factory.insert(:safety_alert_geometry, source: "test")
      geom2 = Factory.insert(:safety_alert_geometry, source: "test")

      source_id = Code.isbn13()

      packet = %{
        "source" => "test",
        "source_id" => source_id,
        "title" => Lorem.sentence(),
        "summary" => Lorem.paragraph(),
        "geometry_source_ids" => [geom1.source_id, geom2.source_id]
      }

      path = alerts_path(conn, :create, "test", source_id)
      conn = put conn, path, packet

      {:ok, conn: conn, source_id: source_id, geom1: geom1, geom2: geom2}
    end

    test "returns 201", %{conn: conn} do
      assert response(conn, 201)
    end

    test "stores the alert record in the database", ctx do
      assert rec = Repo.get_by(Alert, source: "test", source_id: ctx.source_id)
      assert %Geo.MultiPoint{coordinates: coords} = rec.geometry

      assert Enum.member?(coords, ctx.geom2.geometry.coordinates)
      assert Enum.member?(coords, ctx.geom1.geometry.coordinates)
    end
  end

  describe "create alert with bad data" do
    setup %{conn: conn} do
      source_id = Code.isbn13()

      packet = %{
        "source" => "test",
        "source_id" => source_id,
        "title" => Lorem.sentence(),
        "summary" => Lorem.paragraph()
      }

      path = alerts_path(conn, :create, "test", source_id)
      conn = put conn, path, packet

      {:ok, conn: conn, source_id: source_id}
    end

    test "returns 422", %{conn: conn} do
      assert response(conn, 422)
    end

    test "does not store the alert record in the database", ctx do
      refute Repo.get_by(Alert, source: "test", source_id: ctx.source_id)
    end
  end

  describe "start alert import" do
    setup %{conn: conn} do
      Factory.insert_list(5, :safety_alert, source: "test1", imported: true)

      path = alerts_path(conn, :start_import, "test1")
      conn = put conn, path, ""

      {:ok, conn: conn}
    end

    test "returns 204", %{conn: conn} do
      assert response(conn, 204)
    end

    test "sets imported=false for a single source" do
      alerts = Repo.all(from a in Alert, where: a.source == "test1")

      assert Enum.all?(alerts, &(&1.imported == false))
    end
  end

  describe "stop alert import" do
    setup %{conn: conn} do
      Factory.insert_list(5, :safety_alert, source: "test1", imported: true)
      Factory.insert_list(5, :safety_alert, source: "test1", imported: false)

      path = alerts_path(conn, :stop_import, "test1")
      conn = delete conn, path, ""

      {:ok, conn: conn}
    end

    test "returns 204", %{conn: conn} do
      assert response(conn, 204)
    end

    test "removes records with imported=false for a single source" do
      alerts = Repo.all(from a in Alert, where: a.source == "test1")

      assert length(alerts) == 5
      assert Enum.all?(alerts, &(&1.imported == true))
    end
  end
end
