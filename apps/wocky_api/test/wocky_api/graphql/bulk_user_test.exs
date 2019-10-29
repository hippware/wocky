defmodule WockyAPI.GraphQL.BulkUserTest do
  use WockyAPI.GraphQLCase, async: true

  import Mock

  alias Faker.Name
  alias Wocky.Account.User
  alias Wocky.Block
  alias Wocky.DynamicLink.Sandbox, as: DLSandbox
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Roster
  alias Wocky.SMS.Sandbox, as: SMSSandbox

  setup do
    DLSandbox.set_result(:ok)
    SMSSandbox.set_result(:ok)

    [user | users] = Factory.insert_list(6, :user)
    phone_numbers = Enum.map(users, & &1.phone_number)

    {:ok, user: user, users: users, phone_numbers: phone_numbers}
  end

  describe "bulk user lookup" do
    @query """
    query ($phone_numbers: [String!]) {
      userBulkLookup(phone_numbers: $phone_numbers) {
        phone_number
        e164_phone_number
        user { id }
        relationship
      }
    }
    """
    test "should return a list of supplied users", ctx do
      result =
        run_query(@query, ctx.user, %{"phone_numbers" => ctx.phone_numbers})

      refute has_errors(result)

      assert %{
               "userBulkLookup" => results
             } = result.data

      expected =
        ctx.users
        |> Enum.map(
          &%{
            "phone_number" => &1.phone_number,
            "e164_phone_number" => &1.phone_number,
            "user" => %{"id" => &1.id},
            "relationship" => "NONE"
          }
        )
        |> Enum.sort()

      assert expected == Enum.sort(results)
    end

    test "should return an empty user result set for unused numbers", ctx do
      numbers = unused_numbers(ctx.phone_numbers)
      result = run_query(@query, ctx.user, %{"phone_numbers" => numbers})

      refute has_errors(result)

      assert %{
               "userBulkLookup" => results
             } = result.data

      expected =
        numbers
        |> Enum.map(
          &%{
            "phone_number" => &1,
            "e164_phone_number" => &1,
            "user" => nil,
            "relationship" => nil
          }
        )
        |> Enum.sort()

      assert expected == Enum.sort(results)
    end

    test """
         should return a combination when some numbers are used and some unused
         """,
         ctx do
      unused_numbers = unused_numbers(ctx.phone_numbers)
      numbers = unused_numbers ++ ctx.phone_numbers
      result = run_query(@query, ctx.user, %{"phone_numbers" => numbers})

      refute has_errors(result)

      assert %{
               "userBulkLookup" => results
             } = result.data

      expected =
        Enum.map(
          unused_numbers,
          &%{
            "phone_number" => &1,
            "e164_phone_number" => &1,
            "user" => nil,
            "relationship" => nil
          }
        ) ++
          Enum.map(
            ctx.users,
            &%{
              "phone_number" => &1.phone_number,
              "e164_phone_number" => &1.phone_number,
              "user" => %{"id" => &1.id},
              "relationship" => "NONE"
            }
          )

      assert Enum.sort(expected) == Enum.sort(results)
    end

    test "should handle an empty request", ctx do
      result = run_query(@query, ctx.user, %{"phone_numbers" => []})

      refute has_errors(result)

      assert %{
               "userBulkLookup" => []
             } = result.data
    end

    test "should work when requestor's number is included", ctx do
      result =
        run_query(@query, ctx.user, %{
          "phone_numbers" => [ctx.user.phone_number]
        })

      refute has_errors(result)

      assert %{
               "userBulkLookup" => [
                 %{
                   "phone_number" => ctx.user.phone_number,
                   "e164_phone_number" => ctx.user.phone_number,
                   "user" => %{"id" => ctx.user.id},
                   "relationship" => "SELF"
                 }
               ]
             } == result.data
    end

    test "should not return blocked users", ctx do
      blocked = hd(ctx.users)
      Block.block(blocked, ctx.user)

      result =
        run_query(@query, ctx.user, %{"phone_numbers" => [blocked.phone_number]})

      refute has_errors(result)

      assert %{
               "userBulkLookup" => [
                 %{
                   "phone_number" => blocked.phone_number,
                   "e164_phone_number" => blocked.phone_number,
                   "user" => nil,
                   "relationship" => nil
                 }
               ]
             } == result.data
    end

    test "should normalise phone number to E.164", ctx do
      full_number = hd(ctx.phone_numbers)
      {"+1", number} = String.split_at(full_number, 2)

      result = run_query(@query, ctx.user, %{"phone_numbers" => [number]})

      refute has_errors(result)

      assert %{
               "userBulkLookup" => [
                 %{
                   "phone_number" => number,
                   "e164_phone_number" => full_number,
                   "user" => %{
                     "id" => hd(ctx.users).id
                   },
                   "relationship" => "NONE"
                 }
               ]
             } == result.data
    end

    test "should return the correct relationship for target users", ctx do
      [friend, inviter, invitee] = Enum.slice(ctx.users, 0..2)
      Roster.befriend(ctx.user, friend)
      Roster.make_friends(inviter, ctx.user, :disabled)
      Roster.make_friends(ctx.user, invitee, :disabled)

      result =
        run_query(@query, ctx.user, %{
          "phone_numbers" => [
            friend.phone_number,
            inviter.phone_number,
            invitee.phone_number,
            ctx.user.phone_number
          ]
        })

      refute has_errors(result)

      assert %{
               "userBulkLookup" => results
             } = result.data

      expected = [
        %{
          "phone_number" => friend.phone_number,
          "e164_phone_number" => friend.phone_number,
          "user" => %{
            "id" => friend.id
          },
          "relationship" => "FRIEND"
        },
        %{
          "phone_number" => inviter.phone_number,
          "e164_phone_number" => inviter.phone_number,
          "user" => %{
            "id" => inviter.id
          },
          "relationship" => "INVITED_BY"
        },
        %{
          "phone_number" => invitee.phone_number,
          "e164_phone_number" => invitee.phone_number,
          "user" => %{
            "id" => invitee.id
          },
          "relationship" => "INVITED"
        },
        %{
          "phone_number" => ctx.user.phone_number,
          "e164_phone_number" => ctx.user.phone_number,
          "user" => %{
            "id" => ctx.user.id
          },
          "relationship" => "SELF"
        }
      ]

      assert Enum.sort(expected) == Enum.sort(results)
    end

    test "should fail when too many numbers are requested", ctx do
      result =
        run_query(@query, ctx.user, %{
          "phone_numbers" => unused_numbers(ctx.phone_numbers, 101)
        })

      assert has_errors(result)

      assert error_msg(result) =~ "Maximum bulk operation"
    end

    test "duplicate numbers with different formats should succeed", ctx do
      numbers = ["(580) 334-9474", "580-334-9474"]

      result =
        run_query(@query, ctx.user, %{
          "phone_numbers" => numbers
        })

      refute has_errors(result)

      e164 = "+15803349474"

      expected =
        numbers
        |> Enum.map(
          &%{
            "e164_phone_number" => e164,
            "phone_number" => &1,
            "user" => nil,
            "relationship" => nil
          }
        )
        |> Enum.sort()

      assert expected == Enum.sort(result.data["userBulkLookup"])
    end

    test "non-normalisable numbers should return empty results", ctx do
      numbers = ["xxxxx", hd(ctx.phone_numbers)]
      target = hd(ctx.users)

      result =
        run_query(@query, ctx.user, %{
          "phone_numbers" => numbers
        })

      refute has_errors(result)

      expected =
        [
          %{
            "e164_phone_number" => target.phone_number,
            "phone_number" => target.phone_number,
            "user" => %{"id" => target.id},
            "relationship" => "NONE"
          },
          %{
            "phone_number" => "xxxxx",
            "e164_phone_number" => nil,
            "user" => nil,
            "relationship" => nil
          }
        ]
        |> Enum.sort()

      assert expected == Enum.sort(result.data["userBulkLookup"])
    end

    test "should not produce errors on multiple idential input", ctx do
      [n] = unused_numbers(ctx.phone_numbers, 1)

      result =
        run_query(@query, ctx.user, %{
          "phone_numbers" => [n, n]
        })

      refute has_errors(result)

      assert %{
               "userBulkLookup" => [
                 %{
                   "e164_phone_number" => n,
                   "phone_number" => n,
                   "user" => nil,
                   "relationship" => nil
                 }
               ]
             } == result.data
    end
  end

  @query """
  mutation ($input: FriendBulkInviteInput!) {
    friendBulkInvite(input: $input) {
      successful
      result {
        phone_number
        e164_phone_number
        user { id }
        result
        error
      }
    }
  }
  """
  describe "bulk invitations" do
    setup_with_mocks([
      {SMSSandbox, [:passthrough], [send: fn _, _ -> :ok end]}
    ]) do
      :ok
    end

    test "should send a standard invitation for existing users", ctx do
      result =
        run_query(@query, ctx.user, %{
          "input" => %{"phone_numbers" => ctx.phone_numbers}
        })

      refute has_errors(result)

      results = assert_results(result)

      assert length(results) == length(ctx.users)

      Enum.each(
        ctx.users,
        fn u -> assert has_result(results, u, "INTERNAL_INVITATION_SENT") end
      )

      Enum.each(
        ctx.users,
        fn u -> assert Roster.relationship(ctx.user, u) == :invited end
      )
    end

    test "should send SMS invitations to non-existant users", ctx do
      numbers = unused_numbers(ctx.users)

      result =
        run_query(@query, ctx.user, %{"input" => %{"phone_numbers" => numbers}})

      refute has_errors(result)

      results = assert_results(result)

      assert length(results) == length(numbers)

      Enum.each(
        numbers,
        fn u -> assert has_result(results, u, "EXTERNAL_INVITATION_SENT") end
      )

      Enum.each(numbers, fn n ->
        assert_called(SMSSandbox.send(n, :_))
      end)

      assert Repo.get(User, ctx.user.id).smss_sent == length(numbers)
    end

    test "should produce errors for unparsable numbers", ctx do
      number = Name.first_name()

      result =
        run_query(@query, ctx.user, %{"input" => %{"phone_numbers" => [number]}})

      refute has_errors(result)

      results = assert_results(result)

      assert length(results) == 1

      assert has_result(
               results,
               number,
               nil,
               nil,
               "\"The string supplied did not seem to be a phone number\"",
               "COULD_NOT_PARSE_NUMBER"
             )
    end

    test "should send one invitation if numbers parse to the same user", ctx do
      u = hd(ctx.users)
      n = u.phone_number
      {"+1", n2} = String.split_at(n, 2)

      result =
        run_query(@query, ctx.user, %{"input" => %{"phone_numbers" => [n, n2]}})

      refute has_errors(result)

      results = assert_results(result)

      assert length(results) == 2

      assert has_result(results, n, n, u, nil, "INTERNAL_INVITATION_SENT")
      assert has_result(results, n2, n, u, nil, "INTERNAL_INVITATION_SENT")
    end

    test """
         should send one invitation if numbers parse to the same non-user number
         """,
         ctx do
      n = hd(unused_numbers(ctx.users))
      {"+1", n2} = String.split_at(n, 2)
      n3 = n |> String.split_at(5) |> Tuple.to_list() |> Enum.join(" ")

      result =
        run_query(@query, ctx.user, %{
          "input" => %{"phone_numbers" => [n, n2, n3]}
        })

      refute has_errors(result)

      results = assert_results(result)

      assert length(results) == 3

      Enum.each([n, n2, n3], fn x ->
        assert has_result(results, x, n, nil, nil, "EXTERNAL_INVITATION_SENT")
      end)

      assert_called(SMSSandbox.send(n, :_))
      assert not called(SMSSandbox.send(n2, :_))

      assert Repo.get(User, ctx.user.id).smss_sent == 1
    end

    test """
         should work for multiple invitation types in one request
         """,
         ctx do
      n = hd(unused_numbers(ctx.users))
      {"+1", n2} = String.split_at(n, 2)
      u = hd(ctx.users)
      n3 = u.phone_number
      n4 = Name.first_name()

      result =
        run_query(@query, ctx.user, %{
          "input" => %{"phone_numbers" => [n, n2, n3, n4]}
        })

      refute has_errors(result)

      results = assert_results(result)

      assert length(results) == 4

      # SMS result
      assert has_result(results, n, n, nil, nil, "EXTERNAL_INVITATION_SENT")
      assert has_result(results, n2, n, nil, nil, "EXTERNAL_INVITATION_SENT")
      assert_called(SMSSandbox.send(n, :_))
      assert not called(SMSSandbox.send(n2, :_))
      assert Repo.get(User, ctx.user.id).smss_sent == 1

      # Internal invitation result
      assert has_result(results, n3, n3, u, nil, "INTERNAL_INVITATION_SENT")
      assert Roster.relationship(ctx.user, u) == :invited

      # Bad number
      assert has_result(
               results,
               n4,
               nil,
               nil,
               "\"The string supplied did not seem to be a phone number\"",
               "COULD_NOT_PARSE_NUMBER"
             )
    end

    test "should return error for user's own number", ctx do
      result =
        run_query(@query, ctx.user, %{
          "input" => %{"phone_numbers" => [ctx.user.phone_number]}
        })

      refute has_errors(result)

      results = assert_results(result)

      assert length(results) == 1

      assert has_result(
               results,
               ctx.user.phone_number,
               ctx.user.phone_number,
               ctx.user,
               nil,
               "SELF"
             )
    end
  end

  describe "failure of external dynamic link service" do
    setup do
      DLSandbox.set_result({:error, "Failed to generate link"})
    end

    test "should report an error when link generation fails", ctx do
      number = hd(unused_numbers(ctx.users))

      result =
        run_query(@query, ctx.user, %{
          "input" => %{"phone_numbers" => [number]}
        })

      refute has_errors(result)

      results = assert_results(result)

      assert length(results) == 1

      assert has_result(
               results,
               number,
               number,
               nil,
               "\"Failed to generate link\"",
               "SMS_ERROR"
             )
    end
  end

  describe "failures of external SMS service" do
    setup do
      SMSSandbox.set_result({:error, "Failed to send SMS"})
    end

    test "should report an error when link generation fails", ctx do
      number = hd(unused_numbers(ctx.users))

      result =
        run_query(@query, ctx.user, %{
          "input" => %{"phone_numbers" => [number]}
        })

      refute has_errors(result)

      results = assert_results(result)

      assert length(results) == 1

      assert has_result(
               results,
               number,
               number,
               nil,
               "\"Failed to send SMS\"",
               "SMS_ERROR"
             )
    end
  end

  defp assert_results(result) do
    assert %{
             "friendBulkInvite" => %{
               "successful" => true,
               "result" => results
             }
           } = result.data

    results
  end

  defp has_result(results, %User{} = user, result_type) do
    map = %{
      "phone_number" => user.phone_number,
      "e164_phone_number" => user.phone_number,
      "user" => %{"id" => user.id},
      "error" => nil,
      "result" => result_type
    }

    assert Enum.member?(results, map)
  end

  defp has_result(results, phone_number, result_type) do
    map = %{
      "phone_number" => phone_number,
      "e164_phone_number" => phone_number,
      "user" => nil,
      "error" => nil,
      "result" => result_type
    }

    assert Enum.member?(results, map)
  end

  defp has_result(
         results,
         phone_number,
         e164_phone_number,
         user,
         err_string,
         err_result
       ) do
    u =
      case user do
        nil -> nil
        user -> %{"id" => user.id}
      end

    map = %{
      "phone_number" => phone_number,
      "e164_phone_number" => e164_phone_number,
      "user" => u,
      "error" => err_string,
      "result" => err_result
    }

    assert Enum.member?(results, map)
  end

  defp unused_numbers(phone_numbers, count \\ 5) do
    1..count
    |> Enum.map(fn _ -> Factory.phone_number() end)
    |> Enum.uniq()
    |> Kernel.--(phone_numbers)
  end
end
