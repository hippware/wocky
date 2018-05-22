defmodule Wocky.XMPP.Mixfile do
  use Mix.Project

  def project do
    [
      app: :wocky_xmpp,
      version: version(),
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      lockfile: "../../mix.lock",
      elixir: "~> 1.4",
      # If this is set to true, then our ejabberd dependency won't build
      # properly in the prod environment.
      # Mix.env == :prod,
      build_embedded: false,
      start_permanent: Mix.env() == :prod,
      erlc_options: erlc_options(Mix.env()),
      aliases: aliases(),
      deps: deps(),
      preferred_cli_env: [
        ct: :test,
        espec: :test,
        "coveralls.html": :test
      ],
      test_coverage: [tool: ExCoveralls, test_task: "espec"],
      ct: [log_dir: "log/ct", fail_auto_skip: true],
      elvis_config: elvis_config()
    ]
  end

  defp version do
    {ver_result, _} = System.cmd("elixir", ["../../version.exs"])
    ver_result
  end

  defp erlc_options(:test) do
    [{:d, :TEST}, :nowarn_export_all | erlc_options(:dev)]
  end

  defp erlc_options(_) do
    [
      :debug_info,
      :warnings_as_errors,
      :warn_export_vars,
      :warn_obsolete_guard,
      :warn_unused_import,
      {:warn_format, 1},
      {:parse_transform, :lager_transform}
    ]
  end

  # Configuration for the OTP application
  #
  # Type "mix help compile.app" for more information
  def application do
    [
      description: 'JabberWocky XMPP Server',
      # Specify extra applications you'll use from Erlang/Elixir
      extra_applications: [
        :crypto,
        :ssl,
        :runtime_tools
      ],
      included_applications: [
        # This is here because we start it manually and do not want it
        # starting automatically when Wocky starts.
        :mongooseim
      ],
      mod: {:wocky_xmpp_app, []},
      env: [
        wocky_inst: {:system, "WOCKY_INST", "local"},
        iq_crash_response:
          {:system, :atom, "WOCKY_IQ_CRASH_RESPONSE", :error_with_dump}
      ]
    ]
  end

  defp deps do
    [
      {:wocky, in_umbrella: true},
      {:base16, "~> 1.0"},
      {:confex, "~> 3.3"},
      {:honeybadger, "~> 0.6"},
      {:idna, "~> 5.1", override: true},
      {:joken, "~> 1.1"},
      {:lager, "~> 3.6", override: true},
      {:lager_logger, "~> 1.0"},
      {:ranch, "~> 1.5.0", override: true},
      {:timex, "~> 3.1"},
      {:uuid, "~> 1.7", hex: :uuid_erl, override: true},
      {:mongooseim, github: "hippware/mongooseim", branch: "2.1.1/working"},
      {:cowlib, github: "hippware/cowlib", branch: "working", override: true},
      {:erlando, github: "rabbitmq/erlando", branch: "master"},
      {:exml,
       github: "esl/exml", tag: "3.0.1", manager: :rebar3, override: true},
      {:exometer,
       github: "hippware/exometer", branch: "working", override: true},
      {:exometer_core,
       github: "hippware/exometer_core", branch: "working", override: true},
      {:exometer_prometheus,
       github: "GalaxyGorilla/exometer_prometheus",
       branch: "master",
       manager: :rebar3},
      {:fun_chain,
       github: "sasa1977/fun_chain",
       branch: "master",
       runtime: false,
       manager: :rebar3},
      {:stringprep, "1.0.11", override: true},
      {:cache_tab, "1.0.13", override: true},
      {:fast_tls, "1.0.22", override: true},

      # dev dependencies
      {:ex_guard, "~> 1.1", only: :dev, runtime: false},
      {:reprise, "~> 0.5", only: :dev},
      {:mix_elvis,
       github: "hippware/mix_elvis",
       branch: "master",
       runtime: false,
       only: [:dev, :test]},
      {:distillery,
       github: "hippware/distillery",
       branch: "working",
       runtime: false,
       override: true},

      # test dependencies
      {:espec, "~> 1.5", only: :test},
      {:excoveralls, "~> 0.6", only: :test},
      {:meck, "~> 0.8.8", only: :test, runtime: false, override: true},
      {:escalus, github: "hippware/escalus", branch: "patches", only: :test},
      {:mix_ct, github: "hippware/mix_ct", branch: "master", only: :test},
      {:wsecli, ~r//,
       github: "esl/wsecli", branch: "master", override: true, only: :test}
    ]
  end

  defp aliases do
    [
      recompile: ["clean", "compile"],
      prepare: ["deps.get", "compile"],
      lint: ["elvis"],
      test: [],
      testall: ["espec", &reset/1, "ct"]
    ]
  end

  def reset(_) do
    :net_kernel.stop()
    :wocky_xmpp_app.stop()
  end

  defp elvis_config do
    [
      %{
        dirs: ['src'],
        filter: '*.erl',
        rules: [
          {:elvis_style, :line_length, %{limit: 80, skip_comments: false}},
          {:elvis_style, :no_tabs},
          {:elvis_style, :no_trailing_whitespace},
          # We use macros for Elixir module names
          # {:elvis_style, :macro_names},
          # {:elvis_style, :macro_module_names},
          {:elvis_style, :operator_spaces,
           %{rules: [right: ",", right: "++", left: "++"]}},
          {:elvis_style, :nesting_level, %{level: 3}},
          {:elvis_style, :god_modules, %{limit: 25, ignore: []}},
          {:elvis_style, :no_if_expression},
          {:elvis_style, :invalid_dynamic_call},
          {:elvis_style, :used_ignored_variable},
          {:elvis_style, :no_behavior_info},
          {:elvis_style, :module_naming_convention,
           %{regex: "^[a-z]([a-z0-9]*_?)*(_SUITE)?$"}},
          {:elvis_style, :function_naming_convention,
           %{regex: "^([a-z][a-z0-9]*_?)*|'>>='$"}},
          {:elvis_style, :state_record_and_type},
          {:elvis_style, :no_spec_with_records},
          {:elvis_style, :dont_repeat_yourself, %{min_complexity: 15}},
          {:elvis_style, :no_debug_call},
          {:elvis_style, :variable_naming_convention,
           %{regex: "^(_?[A-Z][0-9a-zA-Z]*)$"}}
        ]
      },

      # Slightly less strict rules for tests
      %{
        dirs: ['test'],
        filter: '*.erl',
        rules: [
          {:elvis_style, :line_length, %{limit: 80, skip_comments: false}},
          {:elvis_style, :no_tabs},
          {:elvis_style, :no_trailing_whitespace},
          # We use macros for Elixir module names
          # {:elvis_style, :macro_names},
          # {:elvis_style, :macro_module_names},
          {:elvis_style, :operator_spaces,
           %{rules: [right: ",", right: "++", left: "++"]}},
          {:elvis_style, :nesting_level, %{level: 3}},
          {:elvis_style, :god_modules,
           %{limit: 25, ignore: [:mam_SUITE, :test_helper]}},
          {:elvis_style, :no_if_expression},
          {:elvis_style, :invalid_dynamic_call},
          {:elvis_style, :used_ignored_variable},
          {:elvis_style, :no_behavior_info},
          {:elvis_style, :module_naming_convention,
           %{regex: "^[a-z]([a-z0-9]*_?)*(_SUITE)?$"}},
          {:elvis_style, :function_naming_convention,
           %{regex: "^([a-z][a-z0-9]*_?)*|'>>='$"}},
          {:elvis_style, :state_record_and_type},
          {:elvis_style, :no_spec_with_records},
          {:elvis_style, :dont_repeat_yourself, %{min_complexity: 20}},
          {:elvis_style, :no_debug_call, %{ignore: [:mam_SUITE]}},
          {:elvis_style, :variable_naming_convention,
           %{regex: "^(_?_?[A-Z][0-9a-zA-Z]*)$"}}
        ]
      }
    ]
  end
end
