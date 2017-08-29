defmodule Wocky.XMPP.Mixfile do
  use Mix.Project

  def project do
    [app: :wocky_xmpp,
     version: version(),
     build_path: "../../_build",
     config_path: "../../config/config.exs",
     deps_path: "../../deps",
     lockfile: "../../mix.lock",
     elixir: "~> 1.4",
     # If this is set to true, then our ejabberd dependency won't build
     # properly in the prod environment.
     build_embedded: false, # Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     erlc_options: erlc_options(Mix.env),
     aliases: aliases(),
     deps: deps(),
     preferred_cli_env: [
       ct: :test,
       espec: :test,
       "coveralls.html": :test
     ],
     test_coverage: [tool: ExCoveralls, test_task: "espec"],
     ct: [log_dir: "log/ct"],
     elvis_config: elvis_config()
   ]
  end

  defp version do
    {ver_result, _} = System.cmd("elixir", ["../../version.exs"])
    ver_result
  end

  defp erlc_options(:test), do: [{:d, :TEST} | erlc_options(:dev)]
  defp erlc_options(_) do
    [
      :debug_info,
      :warnings_as_errors,
      # :warn_export_all,
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
    [description: 'JabberWocky XMPP Server',
     # Specify extra applications you'll use from Erlang/Elixir
     extra_applications: [
       :crypto, :ssl, :runtime_tools, :cowboy, :partial, :plug
     ],
     included_applications: [
       # These are here because we start them manually and do not want them
       # starting automatically when Wocky starts.
       :ejabberd
     ],
     mod: {:wocky_xmpp_app, []},
     env: [
       wocky_inst: {:system, "WOCKY_INST", "local"},
       enable_digits_bypass: {:system, :boolean, "WOCKY_ENABLE_BYPASS", true},
       iq_crash_response: {:system, :atom, "WOCKY_IQ_CRASH_RESPONSE",
                           :error_with_dump}
     ]]
  end

  defp deps do
    [
      {:wocky,                in_umbrella: true},
      {:lager,                "~> 3.2",   override: true},
      {:meck,                 "~> 0.8.4", override: true, runtime: false},
      {:hackney,              "~> 1.7",   override: true},
      {:base16,               "~> 1.0",   override: true},
      {:exjsx,                "~> 3.2",   override: true},
      {:timex,                "~> 3.1"},
      {:honeybadger,          "~> 0.6"},
      {:logger_lager_backend, "~> 0.0.2"},
      {:binpp,                "~> 1.1"},
      {:confex,               "~> 2.0"},
      {:espec,                "~> 1.2",    only: :test},
      {:excoveralls,          "~> 0.6",    only: :test},
      {:ex_guard,             "~> 1.1",    only: :dev, runtime: false},
      {:reprise,              "~> 0.5",    only: :dev},
      {:joken,                "~> 1.1"},

      {:ejabberd, github: "hippware/mim-ejabberd", branch: "working-2.0.1-2"},
      {:exometer_core,
        github: "Feuerlabs/exometer_core",
        branch: "master",
        override: true},
      {:exometer,
        github: "Feuerlabs/exometer",
        branch: "master",
        manager: :rebar3,
        override: true},
      {:exometer_cloudwatch,
        github: "hippware/exometer_cloudwatch",
        branch: "master"},
      {:erlando, ~r//,
        github: "rabbitmq/erlando",
        branch: "master",
        override: true},
      {:fun_chain,
        github: "sasa1977/fun_chain",
        branch: "master",
        runtime: false,
        manager: :rebar3},
      {:mix_elvis,
        github: "hippware/mix_elvis",
        branch: "master",
        runtime: false,
        only: [:dev, :test]},
      {:mix_ct,
        github: "hippware/mix_ct",
        branch: "master",
        only: :test},
      {:exref,
        github: "hippware/exref",
        branch: "master"},
      {:escalus,
        github: "hippware/escalus",
        branch: "working",
        only: :test},
      {:certifi,
        github: "hippware/erlang-certifi",
        branch: "working",
        manager: :rebar3,
        override: true},

      # Overrides
      # These are transitive dependencies that need to be overriden to build
      # correctly. They are not used directly by Wocky.
      {:uuid,   "~> 1.6.0", override: true, hex: :uuid_erl},
      {:edown,  "~> 0.8.1", override: true, runtime: false},
      {:folsom, "~> 0.8.3", override: true},
      {:idna,   "~> 5.0",   override: true},
      {:exml,
        github: "esl/exml",
        tag: "2.4.1",
        manager: :rebar3,
        override: true},
      {:syslog,
        github: "Vagabond/erlang-syslog",
        branch: "master",
        override: true,
        manager: :rebar3},
      {:mochijson2, ~r//,
        github: "bjnortier/mochijson2",
        branch: "master",
        override: true},
      {:usec, ~r//,
        github: "esl/usec",
        branch: "master",
        override: true},
      {:cuesport, ~r//,
        github: "esl/cuesport",
        branch: "master",
        override: true},
      {:proper,
        github: "manopapad/proper",
        tag: "v1.2",
        runtime: false,
        override: true},
      {:hamcrest,
        github: "basho/hamcrest-erlang",
        tag: "0.3.0-basho",
        runtime: false,
        override: true},
      {:wsecli, ~r//,
        github: "esl/wsecli",
        branch: "master",
        override: true,
        only: :test}
    ]
  end

  defp aliases do
    [
      recompile: ["clean", "compile"],
      prepare: ["deps.get", "deps.compile goldrush lager", "compile"],
      lint: ["elvis"],
      "test": [],
      testall: ["espec", &reset/1, "ct"]
    ]
  end

  def reset(_) do
    :net_kernel.stop
    :wocky_xmpp_app.stop
  end

  defp elvis_config do
    [
      %{dirs: ['src'],
        filter: '*.erl',
        rules: [
          {:elvis_style, :line_length,
           %{limit: 80, skip_comments: false}},
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
        ]},

      # Slightly less strict rules for tests
      %{dirs: ['test'],
        filter: '*.erl',
        rules: [
          {:elvis_style, :line_length,
           %{limit: 80, skip_comments: false}},
          {:elvis_style, :no_tabs},
          {:elvis_style, :no_trailing_whitespace},
          # We use macros for Elixir module names
          # {:elvis_style, :macro_names},
          # {:elvis_style, :macro_module_names},
          {:elvis_style, :operator_spaces,
           %{rules: [right: ",", right: "++", left: "++"]}},
          {:elvis_style, :nesting_level, %{level: 3}},
          {:elvis_style, :god_modules, %{limit: 25, ignore: [:mam_SUITE,
                                                             :test_helper]}},
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
        ]}
    ]
  end
end
