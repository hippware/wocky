defmodule WockyAPI.LocationAPISpec do
  use ESpec

  alias HTTPoison.Response
  alias Wocky.Repo.Factory
  alias Wocky.Token

  before_all do
    WockyAPI.LocationAPI.start
  end

  defp url(port, user_id) do
    "http://localhost:#{port}/api/v1/users/#{user_id}/location"
  end

  let :port, do: Application.get_env(:wocky_api, :location_api_port)
  let :user, do: Factory.insert(:user, resource: "testing")
  let :token do
    {:ok, {return, _}} = Token.assign(user().id, user().resource)
    return
  end
  let :headers, do: [
    {"Content-Type", "application/json"},
    {"X-Auth-User", user().id},
    {"X-Auth-Token", token()}
  ]
  let :url, do: url(port(), user().id)
  let :payload do
    """
    {
        "location":[{
            "coords":{
                "speed":-1,
                "longitude":-85.7935821931526,
                "latitude":35.17448497921099,
                "accuracy":3000,
                "heading":-1,
                "altitude":271.4361267089844,
                "altitudeAccuracy":10
            },
            "is_heartbeat":false,
            "is_moving":true,
            "odometer":38876926.28380141,
            "uuid":"3B30B2EA-4EF7-4B0F-927A-089921DA86FC",
            "activity":{
                "type":"unknown",
                "confidence":100
            },
            "battery":{
                "level":0.78,
                "is_charging":false
            },
            "timestamp":"2016-10-24T09:45:05.621Z"
        }],
        "resource": "testing"
    }
    """
  end

  describe "Location HTTP API" do
    context "on success" do
      before do
        {:ok, %Response{status_code: code, body: body}} =
          HTTPoison.post(url(), payload(), headers())
        {:shared, code: code, body: body}
      end

      it "should return 204" do
        expect shared.code |> to(eq 204)
      end

      it "should have an empty body" do
        expect shared.body |> to(be_empty())
      end
    end

    context "on failure" do
      it "should return 405 when sent an unknown method" do
        {:ok, %Response{status_code: code}} = HTTPoison.get(url())
        expect code |> to(eq 405)
      end

      it "should return 401 when the auth header is missing" do
        {:ok, %Response{status_code: code}} =
          HTTPoison.post(url(), payload(), [])
        expect code |> to(eq 401)
      end

      it "should return 401 when the auth header and user don't match" do
        {:ok, %Response{status_code: code}} =
          HTTPoison.post(url(port(), "foo"), payload(), headers())
        expect code |> to(eq 401)
      end

      it "should return 400 when the payload is not json" do
        {:ok, %Response{status_code: code}} =
          HTTPoison.post(url(), "bad data", headers())
        expect code |> to(eq 400)
      end

      it "should return 400 when the location array has more than one entry" do
        # This packet has more than one location
        data =
          """
          {
              "location":[{
                  "coords":{
                      "longitude":-85.7935821931526,
                      "latitude":35.17448497921099,
                      "accuracy":3000,
                  }
              },
              {
                  "coords":{
                      "longitude":-85.7935821931526,
                      "latitude":35.17448497921099,
                      "accuracy":3000,
                  }
              }],
              "resource": "testing"
          }
          """
        {:ok, %Response{status_code: code}} =
          HTTPoison.post(url(), data, headers())
        expect code |> to(eq 400)
      end

      context "when the payload is missing keys" do
        it "should return 400 when the payload is missing latitude" do
          data =
            """
            {
                "location":{
                    "coords":{
                        "longitude":-85.7935821931526,
                        "accuracy":3000,
                    }
                },
                "resource": "testing"
            }
            """
          {:ok, %Response{status_code: code}} =
            HTTPoison.post(url(), data, headers())
          expect code |> to(eq 400)
        end

        it "should return 400 when the payload is missing longitude" do
          data =
            """
            {
                "location":{
                    "coords":{
                        "latitude":35.17448497921099,
                        "accuracy":3000
                    }
                },
                "resource": "testing"
            }
            """
          {:ok, %Response{status_code: code}} =
            HTTPoison.post(url(), data, headers())
          expect code |> to(eq 400)
        end

        it "should return 400 when the payload is missing accuracy" do
          data =
            """
            {
                "location":{
                    "coords":{
                        "latitude":35.17448497921099,
                        "longitude":-85.7935821931526
                    }
                },
                "resource": "testing"
            }
            """
          {:ok, %Response{status_code: code}} =
            HTTPoison.post(url(), data, headers())
          expect code |> to(eq 400)
        end

        it "should return 400 when the payload is missing resource" do
          data =
            """
            {
                "location":{
                    "coords":{
                        "latitude":35.17448497921099,
                        "longitude":-85.7935821931526,
                        "accuracy":3000
                    }
                }
            }
            """
          {:ok, %Response{status_code: code}} =
            HTTPoison.post(url(), data, headers())
          expect code |> to(eq 400)
        end
      end
    end
  end
end
