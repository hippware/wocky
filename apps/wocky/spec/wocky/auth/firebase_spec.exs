defmodule Wocky.Auth.FirebaseSpec do
  use ESpec

  import Joken

  alias JOSE.JWK
  alias Timex.Duration
  alias Wocky.Auth.Firebase
  alias Wocky.Auth.FirebaseKeyManager

  @key_id "c947c408c8dd053f7e13117c4e00f0b2b16dc789"

  @private_key """
  -----BEGIN PRIVATE KEY-----
  MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQC3bQRyIwh5Ybw0
  Bd1kNeWh57qSpUKa867rPS3Uwqd42LGRDmC0ZJNBB3mN6J7WNaAO+kwJ1HpGLkQ5
  OtQcKzBmKppr19zm6fAFOFs59fDMHY5aBMYfGaEdqnpGv7+bjbFDEctSwXiOzQ++
  br8871KUPXNE4/8Hjel+HYQO7rSWHuMq3hUBsprfbAUKFoz1VaD7HLBZxg4iEA+p
  jGZ9ykfEfQtIYZSNGjNmZP/XmEzGYF+fGuKgigjiZ6Vvd703r30sFbjUhaaImIOX
  Yk3bt0RYYxBx0CROI+x1aTsEcezQSwKsVeEp/BpemE6MomgEph8rNF3NSP73S3Wu
  QP0jCX3dAgMBAAECggEAad+fJVJbXdSwEUchVupVNXLQGj0RiOcHG/kgLyJ8ECDj
  vVqTLwyugmaSHvsaU4J4dKy8nx/pxACImJAARpIXSaFlqMHcW9zEEf9JiNcQuoCE
  3ijLQsBEYx83nQaozlym6JOozIen0qVCZST/dWiePbqKgkUnu3CKSaU3yHa3/b2z
  eM1tZ/0aCxxTiRn/E0snBBGWMs3XuFcp/nLgw3Nbd1XG6skgOegxqfmCtdNyywzl
  PksfbLepk73axPUbTkkz8WFUOnfrTED4cGo++3VrfaMBLjM1Sm4jiQr9FIB3NmfF
  j4b/Ui3ZNoG1xbjK0xDsuKsl6tLGzNVaFk23PUwkAQKBgQDpyjkrQLlU0FLxg8AS
  k/0lwV7JjMbUQadDr49kQDGOkz/ljlZgStv8jQFsGYpvNIKZi5OXWxNPB3mWQwU3
  z7dF7e47gXCMzvmIrdr1Gs2YPxTYjv9wB3om2D6mLyBs/4yCaGo8hi0SPVYZi2iC
  uQLv3g2UTOKaX9rCmQ0jDQetHQKBgQDI2fA04+drvSQvIFrqk4Skz2g8sN7+7j/n
  yx/aYMJyCNCpvih5L2014xXosTVbNhn5pmFUlZ+4qwwQEzElQgqN2VZdHNxE0bYe
  xTyjSuKkJY0jAcfUJz0neiXbWpKvfj3yQupFNeWjpOE2tsstFfWkH89xL6w/1VIs
  CI/u6M33wQKBgEyHevFSrZg63Xvboesy8GIEi4+0en2OxD8e3/R0IwTF5NuzHUlG
  F/7y9W06axt99+ZlTznzgT2Ud9OdOr8LSrYkbaCi/YHKWtrH9m3XiUd2Fs/Q94Ln
  n6/Jh7CEqrujZ45kuan4ThazZ1TTUrG/+FsmuBE8nczk5cpfqXI42LNtAoGBAJ1/
  gMQIvs0WWUx3I7P0j7wpRATrcUIZE5WxC75Tx8ZiMTYZ/mThEtOByglZBI0MxJum
  o4YPelr2DhSA6DXeLqaC+h0z52ozxIsmgWFO9KBhLeZ4m/k599OADjWPNZ1V8j+J
  x2kUVYnYXh5ogrRNFv1nUGTiTEEWB0SuRifC+NhBAoGAcXiz/l+vO1xa0+qpWFO/
  ZomXwqs+Do40vXqWQg7C7fJ3C3Wgf1RO5MRncVpcawqPkN9jvskFtA18B4lXN6f+
  MRcUPLMy+tbWNleZ/KktLk5GdGeY8GaScFb82EwaWb2NIs1MF/ONyGF5sTZvToK7
  hHku1vi9fpU/eNt0FXgecd8=
  -----END PRIVATE KEY-----
  """

  @cert """
  -----BEGIN CERTIFICATE-----
  MIIDBjCCAe4CCQCgEOA/+QkMRTANBgkqhkiG9w0BAQsFADBFMQswCQYDVQQGEwJB
  VTETMBEGA1UECAwKU29tZS1TdGF0ZTEhMB8GA1UECgwYSW50ZXJuZXQgV2lkZ2l0
  cyBQdHkgTHRkMB4XDTE3MDgyMjA3NDE1OFoXDTQ1MDEwNjA3NDE1OFowRTELMAkG
  A1UEBhMCQVUxEzARBgNVBAgMClNvbWUtU3RhdGUxITAfBgNVBAoMGEludGVybmV0
  IFdpZGdpdHMgUHR5IEx0ZDCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEB
  ALdtBHIjCHlhvDQF3WQ15aHnupKlQprzrus9LdTCp3jYsZEOYLRkk0EHeY3ontY1
  oA76TAnUekYuRDk61BwrMGYqmmvX3Obp8AU4Wzn18MwdjloExh8ZoR2qeka/v5uN
  sUMRy1LBeI7ND75uvzzvUpQ9c0Tj/weN6X4dhA7utJYe4yreFQGymt9sBQoWjPVV
  oPscsFnGDiIQD6mMZn3KR8R9C0hhlI0aM2Zk/9eYTMZgX58a4qCKCOJnpW93vTev
  fSwVuNSFpoiYg5diTdu3RFhjEHHQJE4j7HVpOwRx7NBLAqxV4Sn8Gl6YToyiaASm
  Hys0Xc1I/vdLda5A/SMJfd0CAwEAATANBgkqhkiG9w0BAQsFAAOCAQEAX1k2rWvP
  GJSiLoQuenA3iOT0BmRZYQSG/0uHMFaJOb42Xb3V0F35sjyYDGqH/yDI8/MlA0mq
  /BzkOG+5yQM2F3EjqAnFvO7eFfRHxU0E9xuiweFIx615sYTID6xvvbFFtZ1xD1YV
  EV3G88wSk1g6NYz2BfzpY089JrRNvLApNUk7ssemLOY/FMu+1bI6TNxgn1MU6zHK
  MlV3DJZAUdOZOyx77p4QQVH0BaPWSkNsmUXNQu/8aNbNktuiQN65+ByBPvq6W64u
  oVwftZpq3axVJyOZ9sfISPLUPvsHvK5r24kaSxFKknKO7X8Jb1FUGJ0UidgBi1pf
  hYWUqOAgiBY3RQ==
  -----END CERTIFICATE-----
  """

  @private_key_2 """
  -----BEGIN PRIVATE KEY-----
  MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQDJhRdV/Qpa1aQp
  Zmudc68pWNkbdkYpI700MuRznCJoty/BtFQEiwDQcMtsK4m2zME1JVuzN5DbE6d8
  d87RdMtgHklCeQxgbd5eC/YinXz/FHuP0OvpVzdfi5Mznw7xeDei1EncKcqcvPQw
  qMwuzlUjJ/bjY28OpJXZildLYaw5rZ8yKG8diBjtPaOsC9UNxLZZ6xiKXG/yBHSG
  VibAxwbTBtdPZcPexud9G7FL1QqRZ3DXBymw5l2mWe+h/+tzUQF1JoHLX0s1MI7f
  AHvzcvKsLcQe3g+tqaATke01rGEBKZ5QJD/PJtSiK8wxnS73ywaTBzG7s1Tv5bZI
  GjLVOO+VAgMBAAECggEACsJTU6D165GaeSdhuX9Sq8d1l/TBnDKvSFkrP0znvWWC
  ZDonfjKZsYdEJRtR9/OIoTjEY5Yk4r1y5L9UyHQO46vaR3VFBuT2yVikJIb6BHho
  fG4wGj4w06YVDt2AI75CYiwpYVrN2X5xHyPz+s/DVeQtV0iYxlRdN8ceEAU/97hI
  FaGd6drhebaWxiz4c7eKWq5vnJaId8xi1B/hnMsT3ZYpEJsC5EYRRyCBI+ccvDXA
  cLSac8nYMBvD29ulXpOq7GYXY5YjPjEoGwCH4wqyFU8ypfHmZOXhukAOiI97GsoR
  rkNtMR2tQ8MAV6GEUn7Y301BqR5NvojQi3r8MHQGgQKBgQDkaBaUgxzKO07osHTE
  wrXi6Bge2nqVYvNSmYasq78mHijBzJ26HypWq1PoBEx8loWBq1C4NMGgGguXkSNg
  WWN7RiwMZypPxq49ikIlznel5WsmlSgy7eLlB/ifcGXJ0cB82X+Cw6SsmH+qMHze
  L35j6vbSLK5TGuDjqvYjSaQcYQKBgQDh3XrgkW9hYarMfHM9mrvaYQMFwd60H6Yx
  w6P2tC9Dy3kkvv1WjLFNyYdh8UE/Ff2dogPphJvaTgwPXmDOir5evk4Zqurz8eHG
  TurBSZgUqNDP7/QG1UQpJakyN0OKzP4hgyWL6ZHukngbFKvXn1C0pNXYUxnebsSm
  /JSvFVU/tQKBgQDVZWJjUrQdRZgN0n7mLjydSNbwTJNm6tIHBCVNQhEO3fKaTj+2
  2tNFskBsb0e+xyro4VRDJhcZK/xUmWeB50IItKEG3YQskKXOHVR72hDzUAryOz0Z
  iCQKc44X+H1BN9UtUE/PrlY2Yywzp6WiOKFPXQFJkDE2t9ARJXNduLm5QQKBgFhC
  /mOHMakyS1ZBQ59NOh9qTY4QZoCo5X8I3qfnXZj2iE6Nu934eMjW+eqSJlcKtuLE
  jxbP1T+HxDq1PMs5eI75eveYzNurJ/kgmagSpDVeisrXD9sjt9EEgIu5ANHarWxX
  HXMBQR+jaP/cIhLOlPEigNZaEv/S9VHz3f5VLc0JAoGAdsFoGG2/SE9XRCkQDogt
  osAQGGCDxz7loEt22ZHP6ySpYRBX+z5NKBUo6o0EyPs46OI2lrp4Ekv6QLd1ScxV
  QEDLg14WiQS2m+FdmvRiz4veAOHHJaGAXMn0DSZ1vno+JQigDp5maHVxc0CydFZq
  vm20OdFQ+qZh++T72uGmXxk=
  -----END PRIVATE KEY-----
  """

  @iss "https://securetoken.google.com/"

  @user_id "UserID1"

  @phone_number "+15551231234"

  describe "Firebase auth" do
    before_all do
      :meck.new(FirebaseKeyManager)
      :meck.expect(FirebaseKeyManager, :get_key, fn(@key_id) -> {:ok, @cert}
                                                   (_) -> {:error, :no_key}
      end)
      :ok
    end

    it "should verify valid certifiates" do
      Firebase.verify(make_jwt()) |> should(eq {:ok, {@user_id, @phone_number}})
    end

    it "should fail if the key id is missing or wrong" do
      Firebase.verify(make_jwt(%{kid: nil})) |> should(be_error_result())
      Firebase.verify(make_jwt(%{kid: "abcd"})) |> should(be_error_result())
    end

    it "should fail if the wrong public key is used" do
      Firebase.verify(make_jwt(%{key: @private_key_2}))
      |> should(be_error_result())
    end

    it "should fail if the expiry date is in the past" do
      exp =
        DateTime.utc_now
        |> Timex.subtract(Duration.from_days(1))
        |> Timex.to_unix
      Firebase.verify(make_jwt(%{exp: exp})) |> should(be_error_result())
    end

    it "should fail if the issue date is in the future" do
      iat =
        DateTime.utc_now
        |> Timex.add(Duration.from_days(1))
        |> Timex.to_unix
      Firebase.verify(make_jwt(%{iat: iat})) |> should(be_error_result())
    end

    it "should fail if the audience is not correct" do
      Firebase.verify(make_jwt(%{aud: "wrong_aud"}))
      |> should(be_error_result())
    end

    it "should fail if the issuer is incorrect" do
      Firebase.verify(make_jwt(%{iss: "https://wrong_issuer/wrong"}))
      |> should(be_error_result())
    end

  end


  defp make_jwt(opts \\ %{}) do
    project = Confex.get_env(:wocky, :firebase_project_id)

    params =
      %{exp: DateTime.utc_now
             |> Timex.add(Duration.from_days(1))
             |> Timex.to_unix,
        iat: DateTime.utc_now
             |> Timex.to_unix,
        aud: project,
        iss: @iss <> project,
        sub: @user_id,
        kid: @key_id,
        key: @private_key,
        phone_number: @phone_number
      }
      |> Map.merge(opts)

    jwt =
      %{}
      |> token
      |> with_header_arg("kid", params[:kid])
      |> with_exp(params[:exp])
      |> with_iat(params[:iat])
      |> with_aud(params[:aud])
      |> with_iss(params[:iss])
      |> with_sub(params[:sub])
      |> with_claim("phone_number", params[:phone_number])
      |> with_signer(rs256(JWK.from_pem(params[:key])))
      |> sign
      |> get_compact

    %{"jwt" => jwt}
  end

end
