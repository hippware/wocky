defmodule WockyDbTest do
  use Pavlov.Case
  import Pavlov.Syntax.Expect
  import ExUnit.Callbacks


  describe "start_backend/0" do
    before :each do
      :code.purge(:cassandra_backend)
      :code.delete(:cassandra_backend)
      :wocky_db.start_backend(:seestar)
    end

    it "should generate and load cassandra_backend" do
      assert {:file, _} = :code.is_loaded(:cassandra_backend)
    end

    it "should make cassandra_backend return seestar" do
      expect :cassandra_backend.backend |> to_eq :wocky_db_seestar
    end
  end

  context "backend configuration" do
    context "When the environment has valid configuration data" do
      before :all do
        :meck.new(:wocky_db_seestar)
        :meck.expect(:wocky_db_seestar, :configure,
          fn(:env_host, :env_params) -> :env_configured
            (:par_host, :par_params) -> :par_configured end)

        :meck.new(:application, [:unstick])
        :meck.expect(:application, :get_env,
          fn(:wocky, :host) -> {:ok, :env_host};
            (:wocky, :wocky_db_seestar) -> {:ok, :env_params} end)

        ExUnit.Callbacks.on_exit fn -> :meck.unload end
      end

      describe "maybe_configure/0" do
        it "should call backend with settings from environment" do
          expect :wocky_db.maybe_configure |> to_eq :env_configured
          assert :meck.validate(:application)
          assert :meck.validate(:wocky_db_seestar)
        end
      end

      describe "configure/0" do
        it "should call backend with settings from environment" do
          expect :wocky_db.configure |> to_eq :env_configured
          assert :meck.validate(:application)
          assert :meck.validate(:wocky_db_seestar)
        end
      end

      describe "configure/2" do
        it "should call backend with settings from environment" do
          expect :wocky_db.configure(:par_host, :par_params) |> to_eq :par_configured
          assert :meck.validate(:application)
          assert :meck.validate(:wocky_db_seestar)
        end
      end
    end

    context "When the environment does not have valid configuration data" do
      describe "maybe_configure/0" do
        xit "should return ok if there are no settings in the environment" do
          # pending
        end

        xit "should return ok if there are bad settngs in the environment" do
          # pending
        end
      end

      describe "configure/0" do
        xit "should visibly fail if there are no settings in the environment" do
          # pending
        end

        xit "should visibly fail if there are bad settings in the environment" do
          # pending
        end
      end
    end
  end
end
