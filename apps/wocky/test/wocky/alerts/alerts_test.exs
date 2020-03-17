defmodule Wocky.AlertsTest do
  use Wocky.DataCase, async: true

  alias Faker.Code
  alias Faker.Lorem
  alias Wocky.Alerts
  alias Wocky.Alerts.Alert
  alias Wocky.Alerts.Geometry
  alias Wocky.Repo
  alias Wocky.Repo.Factory

  describe "insert_geometry/1" do
    setup do
      source_id = Code.isbn13()

      data = %{
        source: "test",
        source_id: source_id,
        geometry: %{"type" => "Point", "coordinates" => [100.0, 0.0]}
      }

      result = Alerts.insert_geometry(data)

      {:ok, source_id: source_id, result: result}
    end

    test "returns {:ok, %Geometry{}}", ctx do
      assert {:ok, %Geometry{}} = ctx.result
    end

    test "stores the geometry record in the database", ctx do
      rec = Repo.get_by(Geometry, source: "test", source_id: ctx.source_id)

      assert rec && rec.geometry.coordinates == {100.0, 0.0}
    end

    test "updates an existing geometry record", ctx do
      rec1 = Repo.get_by(Geometry, source: "test", source_id: ctx.source_id)

      data = %{
        source: "test",
        source_id: ctx.source_id,
        geometry: %{"type" => "Point", "coordinates" => [200.0, 1.0]}
      }

      assert {:ok, _} = Alerts.insert_geometry(data)

      rec2 = Repo.get_by(Geometry, source: "test", source_id: ctx.source_id)

      assert rec2.geometry.coordinates == {200.0, 1.0}
      assert Timex.after?(rec2.updated_at, rec1.updated_at)
    end
  end

  describe "insert_alert/1 with embedded geometry" do
    setup do
      source_id = Code.isbn13()

      data = %{
        source: "test",
        source_id: source_id,
        title: Lorem.sentence(),
        summary: Lorem.paragraph(),
        geometry: %{"type" => "Point", "coordinates" => [100.0, 0.0]}
      }

      result = Alerts.insert_alert(data)

      {:ok, source_id: source_id, result: result}
    end

    test "returns {:ok, %Alert{}}", ctx do
      assert {:ok, %Alert{}} = ctx.result
    end

    test "stores the alert record in the database", ctx do
      rec = Repo.get_by(Alert, source: "test", source_id: ctx.source_id)

      assert rec && rec.geometry.coordinates == {100.0, 0.0}
    end

    test "updates an existing alert record", ctx do
      rec1 = Repo.get_by(Alert, source: "test", source_id: ctx.source_id)

      data = %{
        source: "test",
        source_id: ctx.source_id,
        title: Lorem.sentence(),
        summary: Lorem.paragraph(),
        geometry: %{"type" => "Point", "coordinates" => [200.0, 1.0]}
      }

      assert {:ok, _} = Alerts.insert_alert(data)

      rec2 = Repo.get_by(Alert, source: "test", source_id: ctx.source_id)

      assert rec2.geometry.coordinates == {200.0, 1.0}
      assert Timex.after?(rec2.updated_at, rec1.updated_at)
    end
  end

  describe "insert_alert/1 with geometry lookup" do
    setup do
      geom1 = Factory.insert(:safety_alert_geometry, source: "test")
      geom2 = Factory.insert(:safety_alert_geometry, source: "test")

      source_id = Code.isbn13()

      data = %{
        source: "test",
        source_id: source_id,
        title: Lorem.sentence(),
        summary: Lorem.paragraph(),
        geometry_source_ids: [geom1.source_id, geom2.source_id]
      }

      result = Alerts.insert_alert(data)

      {:ok, geom1: geom1, geom2: geom2, source_id: source_id, result: result}
    end

    test "returns {:ok, %Alert{}}", ctx do
      assert {:ok, %Alert{}} = ctx.result
    end

    test "stores the alert record in the database", ctx do
      assert rec = Repo.get_by(Alert, source: "test", source_id: ctx.source_id)
      assert %Geo.MultiPoint{coordinates: coords} = rec.geometry

      assert Enum.member?(coords, ctx.geom1.geometry.coordinates)
      assert Enum.member?(coords, ctx.geom2.geometry.coordinates)
    end

    test "updates an existing alert record", ctx do
      rec1 = Repo.get_by(Alert, source: "test", source_id: ctx.source_id)
      geom3 = Factory.insert(:safety_alert_geometry, source: "test")

      data = %{
        source: "test",
        source_id: ctx.source_id,
        title: Lorem.sentence(),
        summary: Lorem.paragraph(),
        geometry_source_ids: [ctx.geom1.source_id, geom3.source_id]
      }

      assert {:ok, _} = Alerts.insert_alert(data)

      rec2 = Repo.get_by(Alert, source: "test", source_id: ctx.source_id)

      assert %Geo.MultiPoint{coordinates: coords} = rec2.geometry
      assert Enum.member?(coords, ctx.geom1.geometry.coordinates)
      assert Enum.member?(coords, geom3.geometry.coordinates)
      assert Timex.after?(rec2.updated_at, rec1.updated_at)
    end
  end

  describe "alert_import_begin/1" do
    setup do
      Factory.insert_list(5, :safety_alert, source: "test1", imported: true)
      Factory.insert_list(5, :safety_alert, source: "test2", imported: true)

      :ok = Alerts.alert_import_begin("test1")
    end

    test "sets imported=false for a single source" do
      alerts = Repo.all(from a in Alert, where: a.source == "test1")

      assert Enum.all?(alerts, &(&1.imported == false))
    end

    test "does not set imported flag for other sources" do
      alerts = Repo.all(from a in Alert, where: a.source == "test2")

      assert Enum.all?(alerts, &(&1.imported == true))
    end
  end

  describe "alert_import_end/1" do
    setup do
      Factory.insert_list(5, :safety_alert, source: "test1", imported: true)
      Factory.insert_list(5, :safety_alert, source: "test1", imported: false)
      Factory.insert_list(5, :safety_alert, source: "test2", imported: true)
      Factory.insert_list(5, :safety_alert, source: "test2", imported: false)

      :ok = Alerts.alert_import_end("test1")
    end

    test "removes records with imported=false for a single source" do
      alerts = Repo.all(from a in Alert, where: a.source == "test1")

      assert length(alerts) == 5
      assert Enum.all?(alerts, &(&1.imported == true))
    end

    test "does not remove records from another source" do
      alerts = Repo.all(from a in Alert, where: a.source == "test2")

      assert length(alerts) == 10
      refute Enum.all?(alerts, &(&1.imported == true))
    end
  end
end
