defmodule WockyAPI.GraphQL.UserInviteTest do
  use WockyAPI.GraphQLCase, async: true

  import Mock

  alias Faker.Name
  alias Wocky.Account.User
  alias Wocky.Friends
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.SMS.Sandbox, as: SMSSandbox
  alias Wocky.UserInvite
  alias Wocky.UserInvite.DynamicLink.Sandbox, as: DLSandbox

  setup do
    user = Factory.insert(:user)

    {:ok, user: user}
  end

  # -------------------------------------------------------------------
  # Mutations

  describe "userInviteSend mutation" do
    @query """
    mutation ($input: UserInviteSendInput!) {
      userInviteSend(input: $input) {
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

    setup do
      DLSandbox.set_result(:ok)
      SMSSandbox.set_result(:ok)

      :ok
    end

    test "should send a standard invitation for existing users", ctx do
      other_user = Factory.insert(:user)
      phone_number = other_user.phone_number
      user_id = other_user.id

      result =
        run_query(@query, ctx.user, %{
          "input" => %{
            "phone_number" => phone_number,
            "share_type" => "ALWAYS"
          }
        })

      refute has_errors(result)

      assert %{
               "userInviteSend" => %{
                 "successful" => true,
                 "result" => %{
                   "phone_number" => ^phone_number,
                   "e164_phone_number" => ^phone_number,
                   "user" => %{"id" => ^user_id},
                   "result" => "INTERNAL_INVITATION_SENT"
                 }
               }
             } = result.data
    end

    test "should send SMS invitations to non-existant users", ctx do
      phone_number = Factory.phone_number()

      result =
        run_query(@query, ctx.user, %{
          "input" => %{
            "phone_number" => phone_number,
            "share_type" => "ALWAYS"
          }
        })

      refute has_errors(result)

      assert %{
               "userInviteSend" => %{
                 "successful" => true,
                 "result" => %{
                   "phone_number" => ^phone_number,
                   "e164_phone_number" => ^phone_number,
                   "user" => nil,
                   "result" => "EXTERNAL_INVITATION_SENT"
                 }
               }
             } = result.data
    end

    test "should produce errors for unparsable numbers", ctx do
      phone_number = Name.first_name()

      result =
        run_query(@query, ctx.user, %{
          "input" => %{
            "phone_number" => phone_number,
            "share_type" => "ALWAYS"
          }
        })

      refute has_errors(result)

      assert %{
               "userInviteSend" => %{
                 "successful" => true,
                 "result" => %{
                   "phone_number" => ^phone_number,
                   "e164_phone_number" => nil,
                   "user" => nil,
                   "result" => "COULD_NOT_PARSE_NUMBER"
                 }
               }
             } = result.data
    end

    test "should return error for user's own number", ctx do
      phone_number = ctx.user.phone_number
      user_id = ctx.user.id

      result =
        run_query(@query, ctx.user, %{
          "input" => %{
            "phone_number" => phone_number,
            "share_type" => "ALWAYS"
          }
        })

      refute has_errors(result)

      assert %{
               "userInviteSend" => %{
                 "successful" => true,
                 "result" => %{
                   "phone_number" => ^phone_number,
                   "e164_phone_number" => ^phone_number,
                   "user" => %{"id" => ^user_id},
                   "result" => "SELF"
                 }
               }
             } = result.data
    end

    test "should report an error when sending via SMS fails", ctx do
      SMSSandbox.set_result({:error, "Failed to send SMS"})

      phone_number = Factory.phone_number()

      result =
        run_query(@query, ctx.user, %{
          "input" => %{
            "phone_number" => phone_number,
            "share_type" => "ALWAYS"
          }
        })

      refute has_errors(result)

      assert %{
               "userInviteSend" => %{
                 "successful" => true,
                 "result" => %{
                   "phone_number" => ^phone_number,
                   "e164_phone_number" => ^phone_number,
                   "user" => nil,
                   "result" => "SMS_ERROR",
                   "error" => ~s|"Failed to send SMS"|
                 }
               }
             } = result.data
    end

    test "should report an error when link generation fails", ctx do
      DLSandbox.set_result({:error, "Failed to generate link"})

      phone_number = Factory.phone_number()

      result =
        run_query(@query, ctx.user, %{
          "input" => %{
            "phone_number" => phone_number,
            "share_type" => "ALWAYS"
          }
        })

      refute has_errors(result)

      assert %{
               "userInviteSend" => %{
                 "successful" => true,
                 "result" => %{
                   "phone_number" => ^phone_number,
                   "e164_phone_number" => ^phone_number,
                   "user" => nil,
                   "result" => "SMS_ERROR",
                   "error" => ~s|"Failed to generate link"|
                 }
               }
             } = result.data
    end
  end

  describe "userInviteRedeemCode mutation" do
    @query """
    mutation ($code: String!) {
      userInviteRedeemCode(input: {code: $code}) {
        result
      }
    }
    """

    test "redeem invitation code", %{user: user} do
      inviter = Factory.insert(:user)
      {:ok, code} = UserInvite.make_code(inviter, user.phone_number, :always)

      result = run_query(@query, user, %{"code" => code})
      refute has_errors(result)
      assert result.data == %{"userInviteRedeemCode" => %{"result" => true}}
    end
  end

  # DEPRECATED
  describe "userInviteMakeCode mutation" do
    @query """
    mutation {
      userInviteMakeCode {
        successful
        result
      }
    }
    """

    test "get invitation code", %{user: user} do
      result = run_query(@query, user)

      refute has_errors(result)

      assert %{
               "userInviteMakeCode" => %{
                 "successful" => true,
                 "result" => code
               }
             } = result.data

      assert is_binary(code)
      assert byte_size(code) > 1
    end
  end

  # DEPRECATED
  describe "friendBulkInvite mutation" do
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

    setup do
      DLSandbox.set_result(:ok)
      SMSSandbox.set_result(:ok)

      users = Factory.insert_list(5, :user)
      phone_numbers = Enum.map(users, & &1.phone_number)

      {:ok, users: users, phone_numbers: phone_numbers}
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
        fn u -> assert Friends.relationship(ctx.user, u) == :invited end
      )
    end

    test "should send SMS invitations to non-existant users", ctx do
      with_mock(SMSSandbox, [:passthrough], send: fn _, _ -> :ok end) do
        numbers = unused_numbers(ctx.users)

        result =
          run_query(@query, ctx.user, %{
            "input" => %{"phone_numbers" => numbers}
          })

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
      with_mock(SMSSandbox, [:passthrough], send: fn _, _ -> :ok end) do
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
    end

    test "should work for multiple invitation types in one request", ctx do
      with_mock(SMSSandbox, [:passthrough], send: fn _, _ -> :ok end) do
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
        assert Friends.relationship(ctx.user, u) == :invited

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

    test "should report an error when sending via SMS fails", ctx do
      SMSSandbox.set_result({:error, "Failed to send SMS"})

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

    test "should report an error when link generation fails", ctx do
      DLSandbox.set_result({:error, "Failed to generate link"})

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
