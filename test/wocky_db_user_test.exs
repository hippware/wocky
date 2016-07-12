defmodule WockyDbUserTest do
  use Pavlov.Case

  let :domain, do: "localhost"
  let :user, do: "bob"
  let :pass, do: "password"

  before :all do
    :ok = :wocky_app.start
    ExUnit.Callbacks.on_exit fn -> :wocky_app.stop end
    :ok
  end

  def insert_test_data do
    uuid = :ossp_uuid.make(:v1, :binary)
    query1 = "INSERT INTO username_to_user (id, domain, username) VALUES (?, ?, ?)"
    {:ok, _} = :wocky_db.pquery(:shared, query1, [uuid, domain, user], :quorum)

    query2 = "INSERT INTO user (id, domain, username, password) VALUES (?, ?, ?, ?)"
    {:ok, _} = :wocky_db.pquery(domain, query2, [uuid, domain, user, pass], :quorum)
  end

  def clean_database do
    {:ok, _} = :wocky_db.pquery(:shared, "TRUNCATE username_to_user", [], :quorum)
    {:ok, _} = :wocky_db.pquery(domain, "TRUNCATE user", [], :quorum)
  end

  describe "does_user_exist/2" do
    before :all do
      WockyDbUserTest.insert_test_data
      ExUnit.Callbacks.on_exit fn -> WockyDbUserTest.clean_database end
      :ok
    end

    it "returns true if the user exists" do
      assert :wocky_db_user.does_user_exist(domain, user)
    end

    it "returns false if the user does not exist" do
      assert not :wocky_db_user.does_user_exist(domain, "doesnotexist")
    end
  end

  describe "create_user/3" do
    before :all do
      WockyDbUserTest.insert_test_data
      ExUnit.Callbacks.on_exit fn -> WockyDbUserTest.clean_database end
      :ok
    end

    it "creates a user if one does not exist" do
      assert :ok == :wocky_db_user.create_user(domain, "alice", pass)
      assert :wocky_db_user.does_user_exist(domain, "alice")
    end

    it "returns {error, exists} if the user already exists" do
      assert {:error, :exists} == :wocky_db_user.create_user(domain, user, pass)
    end
  end

  describe "get_password/2" do
    before :all do
      WockyDbUserTest.insert_test_data
      ExUnit.Callbacks.on_exit fn -> WockyDbUserTest.clean_database end
      :ok
    end

    it "returns a password if the user exists" do
      assert pass == :wocky_db_user.get_password(domain, user)
    end

    it "returns {error, not_found} if the user does not exist" do
      assert {:error, :not_found} == :wocky_db_user.get_password(domain, "baduser")
    end
  end

  describe "set_password/3" do
    before :all do
      WockyDbUserTest.insert_test_data
      ExUnit.Callbacks.on_exit fn -> WockyDbUserTest.clean_database end
      :ok
    end

    it "sets the user's password if the user exists" do
      assert :ok == :wocky_db_user.set_password(domain, user, "newpass")
      assert "newpass" == :wocky_db_user.get_password(domain, user)
    end

    it "returns {error, not_found} if the user does not exist" do
      assert {:error, :not_found} == :wocky_db_user.set_password(domain, "baduser", pass)
    end
  end

  describe "remove_user/2" do
    before :all do
      WockyDbUserTest.insert_test_data
      ExUnit.Callbacks.on_exit fn -> WockyDbUserTest.clean_database end
      :ok
    end

    it "removes the user if they exist" do
      assert :ok == :wocky_db_user.remove_user(domain, user)
      assert not :wocky_db_user.does_user_exist(domain, user)
    end

    it "returns ok if the user does not exist" do
      assert :ok == :wocky_db_user.remove_user(domain, "doesnotexist")
    end
  end
end
