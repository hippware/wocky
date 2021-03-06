defmodule WockyAPI.Controllers.GeometriesControllerTest do
  use WockyAPI.ConnCase

  alias Faker.Code
  alias Wocky.Alerts.Geometry
  alias Wocky.Repo

  setup %{conn: conn} do
    conn = put_req_header(conn, "content-type", "application/json")

    {:ok, conn: conn}
  end

  describe "create geometry" do
    setup %{conn: conn} do
      source_id = Code.isbn13()

      packet = %{
        "source" => "test",
        "source_id" => source_id,
        "geometry" => %{"type" => "Point", "coordinates" => [100.0, 0.0]}
      }

      path = geometries_path(conn, :create, "test", source_id)
      conn = put conn, path, packet

      {:ok, conn: conn, source_id: source_id}
    end

    test "returns 201", %{conn: conn} do
      assert response(conn, 201)
    end

    test "stores the geometry record in the database", ctx do
      rec = Repo.get_by(Geometry, source: "test", source_id: ctx.source_id)

      assert rec && rec.geometry.coordinates == {100.0, 0.0}
    end
  end

  describe "create geometry with bad data" do
    setup %{conn: conn} do
      source_id = Code.isbn13()

      packet = %{
        "source" => "test",
        "source_id" => source_id
      }

      path = geometries_path(conn, :create, "test", source_id)
      conn = put conn, path, packet

      {:ok, conn: conn, source_id: source_id}
    end

    test "returns 422", %{conn: conn} do
      assert response(conn, 422)
    end

    test "does not store the geometry record in the database", ctx do
      refute Repo.get_by(Geometry, source: "test", source_id: ctx.source_id)
    end
  end
end
