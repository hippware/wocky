defmodule Wocky.Auth.FirebaseKeyManagerTest do
  use ExUnit.Case, async: false
  use ExVCR.Mock, adapter: ExVCR.Adapter.Hackney

  alias ExVCR.Setting

  import Wocky.Auth.FirebaseKeyManager

  @id1 "3450b0ba9e31722e090ed2135bcdb7c177a32f27"

  @cert1 """
-----BEGIN CERTIFICATE-----
MIIDHDCCAgSgAwIBAgIIQsUUSIWD+jwwDQYJKoZIhvcNAQEFBQAwMTEvMC0GA1UE
AxMmc2VjdXJldG9rZW4uc3lzdGVtLmdzZXJ2aWNlYWNjb3VudC5jb20wHhcNMTcw
ODE3MDA0NTI2WhcNMTcwODIwMDExNTI2WjAxMS8wLQYDVQQDEyZzZWN1cmV0b2tl
bi5zeXN0ZW0uZ3NlcnZpY2VhY2NvdW50LmNvbTCCASIwDQYJKoZIhvcNAQEBBQAD
ggEPADCCAQoCggEBAOXC/+IqxPoPZs5PZF3UtG7lN+sisDATauyPb9oysDsD1/e3
ingpDGDZrMTceNUuZosK2Ivn+iopWCNRBGmTcMUlNFMOzlQWnY2APUn/Jf0fkxfG
uw7syQS6eFq1+gd/qclXG78pF3BAk5C0g1zfplgRUi3oT646Cl5Lm59njYpcyptU
Mo5LmvzxWxtjNHChH3aRDx/cZDzrcVyDwQ31vxERTjb/RkzQkHdSSWrE32l7CcrR
YqEFuuayWBW7MTOtODqIGmFpi/qBdzMw6xzKVaFGAlos8xbcXug07dA8tScHFciM
6FKzHsKNm+/ASgjIkwob9am7XKr3hFqtPzpLT/sCAwEAAaM4MDYwDAYDVR0TAQH/
BAIwADAOBgNVHQ8BAf8EBAMCB4AwFgYDVR0lAQH/BAwwCgYIKwYBBQUHAwIwDQYJ
KoZIhvcNAQEFBQADggEBAHsKM9xugA8gTp9Yh5DB12xMnzkYJv3GrfVOG4bUqJ3b
xJQLCXD6PWBBM3mhbfbf7z2IOaPQlgXSTdfkcj2i61Ivas7BeDbUN6Vnxaqoo7tP
6UI+mjEPLGP2kN30+jvfjg4isi2on+cTdJjopmMY5jIk8eGBpIANzgNkIi6yaA+r
C4tKVi3kSVgPnrVDIeBMuVux5nSct2ZZmrZrcsFS9LoVxYXJouie3irSFIy6e8gB
N8yMZY1SH3FDu/288FHCQ6lkzGJ3fMfhGG/xXgNPRQgekukZnDIZOCev30JLYAGB
wHl3KkZYe+Q2g97N3lkd4ZN0yJm4WTTdCVbZuWTrl+k=
-----END CERTIFICATE-----
  """

  @id2 "84b68b30be86fc92f80f51f671378c0b776f69a9"

  @cert2 """
-----BEGIN CERTIFICATE-----
MIIDHDCCAgSgAwIBAgIIAtfEkZTK8NEwDQYJKoZIhvcNAQEFBQAwMTEvMC0GA1UE
AxMmc2VjdXJldG9rZW4uc3lzdGVtLmdzZXJ2aWNlYWNjb3VudC5jb20wHhcNMTcw
ODE2MDA0NTI2WhcNMTcwODE5MDExNTI2WjAxMS8wLQYDVQQDEyZzZWN1cmV0b2tl
bi5zeXN0ZW0uZ3NlcnZpY2VhY2NvdW50LmNvbTCCASIwDQYJKoZIhvcNAQEBBQAD
ggEPADCCAQoCggEBAPNZwpcUh4oHDEPbjvbr7AyduJLWNAn9q5MVEsgSi0xDTPW2
3ZsIG2poIwyNNT/k8pq/AgythJkxoUozmVywL56myPqKuGk8kSk593CqPI7LAAR5
OyoeuHBvwguOs5QjzfLFECewb56xqyKwLzEuGTUikGzsM79GL4a8ycOZ7+VTCM3Q
Ud9+rmiRVOvIeSVxtkwvrUgTP5oLlFdTRoUyYdhv17Ak4k3ToN/gvpLRXLQe+HBy
hPuPTd4lpXRF15GGsevymI4+4KGJpKRiyKferLQSvJMUEdRGswHDf7fkoPIHWJez
tH1bCszUnNDk63SBXqtT63F1lIv0TCkP6qERP8kCAwEAAaM4MDYwDAYDVR0TAQH/
BAIwADAOBgNVHQ8BAf8EBAMCB4AwFgYDVR0lAQH/BAwwCgYIKwYBBQUHAwIwDQYJ
KoZIhvcNAQEFBQADggEBAMfFIXkKw6+KuMVm9ohuppt22wczVHGy0H0FX/XeM8S1
ya1CQUo1iqkuamUJjyGUT+pgsZFPIf2F9Wv15fgS2WKhp7FtaQd872NehLPqMOyQ
xoV+pJlDOpgQdrmVC7iPogujAuq+kgHiFRbsEZa/Khc/9+hbU3jmY2a0YgPJtA8/
0/szawXH1fZ0hVYtzfKLgLlNY/nEycD4TufNqljDXEVboumxkTWKkSxtZNNQBmng
pUKmWJZe2puhUh+CiAq+sGmVeAE+dGv0zXuPDfNLSOX3w+XWTmirp+qe2M8gavjW
jVqILtFtaDcNqZO3mRW3Se5oKNR34BOMRaJ3lMGTILc=
-----END CERTIFICATE-----
  """

  @oldid "OLDID"

  @oldcert "OLDCERT1234"

  test "startup" do
    use_cassette "firebase_startup" do
      force_reload()
      assert get_key(@id1) == @cert1
      assert get_key(@id2) == @cert2
    end
  end

  test "reload timeout" do
    use_cassette "firebase_reload", custom: true do
      force_reload()
    end
    use_cassette "firebase_startup" do
      assert get_key(@id1) == @cert1
      assert get_key(@id2) == nil
      assert get_key(@oldid) == @oldcert
      Process.sleep(1500)
      assert get_key(@id1) == @cert1
      assert get_key(@id2) == @cert2
      assert get_key(@oldid) == nil
    end
  end
end

