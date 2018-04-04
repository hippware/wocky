defmodule WockyAPI.UserTest do
  use WockyAPI.ConnCase, async: true

  alias Faker.String
  alias Wocky.Account
  alias Wocky.Blocking
  alias Wocky.Repo.Factory
  alias Wocky.Repo
  alias Wocky.Repo.ID
  alias Wocky.User.Location

  setup do
    [user, user2] = Factory.insert_list(2, :user)
    {:ok, {token, _}} = Account.assign_token(user.id, "abc")
    conn =
      build_conn()
      |> put_req_header("x-auth-user", user.id)
      |> put_req_header("x-auth-token", token)
    {:ok, user: user, user2: user2, conn: conn}
  end

  @query """
  {
    currentUser {
      id
      firstName
    }
  }
  """
  test "get basic current user info", %{user: user, conn: conn} do
    assert post_conn(conn, @query, 200) ==
      %{
        "data" => %{
          "currentUser" => %{
            "id" => user.id,
            "firstName" => user.first_name
          }
        }
      }
  end

  @query """
  query ($id: String!) {
    user (id: $id) {
      id
      handle
    }
  }
  """
  test "get other user info", %{user2: user2, conn: conn} do
    assert post_conn(conn, @query, %{id: user2.id}, 200) ==
      %{
        "data" => %{
          "user" => %{
            "id" => user2.id,
            "handle" => user2.handle
          }
        }
      }
  end

  test "non-existant ID", %{conn: conn} do
    assert %{
      "data" => %{
        "user" => nil
      },
      "errors" => _
    } = post_conn(conn, @query, %{id: ID.new()}, 200)
  end

  test "invalid ID", %{conn: conn} do
    assert %{
      "errors" => _
    } = post_conn(conn, @query, %{id: "not_an_id"}, 400)
  end

  test "blocked user", %{user: user, user2: user2, conn: conn} do
    Blocking.block(user2, user)
    assert %{
      "data" => %{
        "user" => nil
      },
      "errors" => _
    } = post_conn(conn, @query, %{id: user2.id}, 200)
  end

  describe "user bots" do
    setup %{user: user, user2: user2} do
      bot = Factory.insert(:bot, user: user)
      bot2 = Factory.insert(:bot, user: user2, public: true)

      {:ok, bot: bot, bot2: bot2}
    end

    @query """
    query ($id: String!) {
      user (id: $id) {
        bots (first: 1, relationship: OWNED) {
          totalCount
          edges {
            node {
              id
            }
          }
        }
      }
    }
    """
    test "get by owner", %{conn: conn, user2: user2, bot2: bot2} do
      assert post_conn(conn, @query, %{id: user2.id}, 200) ==
        %{
          "data" => %{
            "user" => %{
              "bots" => %{
                "totalCount" => 1,
                "edges" => [%{
                  "node" => %{
                    "id" => bot2.id
                  }
                }]
              }
            }
          }
        }
    end

    @query """
    {
      currentUser {
        bots (first: 1, relationship: OWNED) {
          totalCount
          edges {
            node {
              id
            }
          }
        }
      }
    }
    """
    test "get own bots", %{conn: conn, bot: bot} do
      assert post_conn(conn, @query, 200) ==
        %{
          "data" => %{
            "currentUser" => %{
              "bots" => %{
                "totalCount" => 1,
                "edges" => [%{
                  "node" => %{
                    "id" => bot.id
                  }
                }]
              }
            }
          }
        }
    end

  end

  @query """
  mutation ($lat: Float!, $lon: Float!, $accuracy: Float!, $resource: String!) {
    setLocation (location: {lat: $lat, lon: $lon, accuracy: $accuracy, resource: $resource}) {
      successful
    }
  }
  """
  test "set location", %{conn: conn, user: user} do
    lat = :rand.uniform() * 89.0
    lon = :rand.uniform() * 179.0
    accuracy = :rand.uniform() * 10.0
    resource = String.base64()

    assert post_conn(
      conn, @query, %{lat: lat, lon: lon,
        accuracy: accuracy, resource: resource}, 200) ==
        %{
          "data" => %{
            "setLocation" => %{
              "successful" => true
            }
          }
        }
    assert %Location{
      lat: ^lat, lon: ^lon, resource: ^resource, accuracy: ^accuracy} =
      Repo.get_by(Location, user_id: user.id)
  end
  test "invalid location", %{conn: conn, user: user} do
    lat = :rand.uniform() * 89.0
    lon = :rand.uniform() * 179.0

    assert post_conn(
      conn, @query, %{lat: lat, lon: lon,
        accuracy: -1.0, resource: String.base64()}, 200) ==
        %{
          "data" => %{
            "setLocation" => %{
              "successful" => false
            }
          }
        }
   assert Repo.get_by(Location, user_id: user.id) == nil
  end
end
