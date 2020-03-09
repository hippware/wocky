defmodule Wocky.AlertsTest do
  use Wocky.DataCase, async: true

  alias Faker.Code
  alias Faker.Lorem
  alias Wocky.Alerts
  alias Wocky.Alerts.Alert
  alias Wocky.Alerts.Geometry
  alias Wocky.Repo

  describe "insert_geometry/1" do
    setup do
      source_id = Code.isbn13()

      data = %{
        "source" => "test",
        "source_id" => source_id,
        "geometry" => %{"type" => "Point", "coordinates" => [100.0, 0.0]}
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
        "source" => "test",
        "source_id" => ctx.source_id,
        "geometry" => %{"type" => "Point", "coordinates" => [200.0, 1.0]}
      }

      assert {:ok, _} = Alerts.insert_geometry(data)

      rec2 = Repo.get_by(Geometry, source: "test", source_id: ctx.source_id)

      assert rec2.geometry.coordinates == {200.0, 1.0}
      assert Timex.after?(rec2.updated_at, rec1.updated_at)
    end
  end

  describe "insert_alert/1" do
    setup do
      source_id = Code.isbn13()

      data = %{
        "source" => "test",
        "source_id" => source_id,
        "title" => Lorem.sentence(),
        "summary" => Lorem.paragraph(),
        "geometry" => %{"type" => "Point", "coordinates" => [100.0, 0.0]}
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
        "source" => "test",
        "source_id" => ctx.source_id,
        "title" => Lorem.sentence(),
        "summary" => Lorem.paragraph(),
        "geometry" => %{"type" => "Point", "coordinates" => [200.0, 1.0]}
      }

      assert {:ok, _} = Alerts.insert_alert(data)

      rec2 = Repo.get_by(Alert, source: "test", source_id: ctx.source_id)

      assert rec2.geometry.coordinates == {200.0, 1.0}
      assert Timex.after?(rec2.updated_at, rec1.updated_at)
    end
  end
end
