defmodule Wocky.Mixfile do
  use Mix.Project

  def project do
    [app: :wocky,
     version: version,
     elixir: "~> 1.3",
     # build_embedded: Mix.env == :prod,
     # start_permanent: Mix.env == :prod,
     erlc_options: erlc_options(Mix.env),
     test_coverage: [output: "_build/#{Mix.env}/cover"],
     aliases: aliases,
     deps: deps,
     preferred_cli_env: [release: :prod,
                         espec:   :test,
                         eunit:   :test,
                         ct:      :test,
                         'db.dump.test':     :test,
                         'db.load.test':     :test,
                         'db.reset.test':    :test,
                         'db.migrate.test':  :test,
                         'db.rollback.test': :test,
                         'db.test_migrations': :test
                       ],
     dialyzer: [
       plt_apps: [
         :compiler, :crypto, :erts, :kernel, :stdlib, :mnesia, :ssl, :ssh,
         :xmerl, :public_key, :tools, :sasl, :hipe, :edoc, :syntax_tools,
         :runtime_tools, :inets, :asn1, :cowboy, :cowlib, :exml, :p1_utils,
         :binpp, :pretty_errors, :mochijson2, :erlando, :z_stdlib, :uuid,
         :cqerl, :erlang_murmurhash, :timex, :ejabberd, :lager, :ossp_uuid,
         :algolia, :logger, :schemata, :porcelain, :geocalc, :mix, :faker,
         :ex_machina, :base16, :poison, :ex_aws, :exconstructor, :honeybadger
       ],
       plt_add_deps: true,
       flags: [
         "--fullpath", "-Wunmatched_returns", "-Werror_handling",
         "-Wrace_conditions", "-Wunderspecs", "-Wunknown"
       ]
     ],
     elvis_config: elvis_config
   ]
  end

  defp version do
    {version, _} = System.cmd "bash", ["./version"]
    version
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

  def application do
    dev_apps = Mix.env == :dev && [:reprise] || []
    [description: 'JabberWocky XMPP Server',
     applications: dev_apps ++ [
       :crypto, :ssl, :lager, :logger, :algolia, :ex_aws, :geocalc, :hackney,
       :poison, :idna, :runtime_tools, :cache_tab, :alarms, :setup, :porcelain,
       :ex_machina, :faker, :cowboy, :honeybadger
     ],
     included_applications: [
       :schemata, :ejabberd, :ossp_uuid, :z_stdlib, :mochijson2,
       :erlando, :logger_lager_backend, :lager_syslog, :syslog,
       :exconstructor, :erlware_commons,

       # ejabberd dependencies that aren't listed in ejabberd.app
       :fusco, :p1_utils, :cuesport, :base16, :xmerl, :usec, :redo,

       # Runtime tools
       :recon, :eper, :binpp, :pretty_errors
     ],
     mod: {:wocky_app, []},
     env: [
       wocky_env: 'dev',
       config_dir: 'etc',
       francus_chunk_size: 1048576, # 1MB
       keyspace_prefix: 'wocky_test_',
       location_api_port: 8080,
       indexing_enabled_envs: ['staging'],
       algolia_index_name: 'dev_wocky_users',
       notification_enabled_envs: ['staging'],
       notification_handler: Wocky.Notification.NullHandler
     ]]
  end

  defp deps do
    [
      {:setup,         "1.7.0", override: true},
      {:lager,         "~> 3.2", override: true},
      {:porcelain,     "~> 2.0"},
      {:algolia,       "~> 0.4.0"},
      {:ex_aws,        "~> 1.0.0-rc.3"},
      {:geocalc,       "~> 0.5"},
      {:exconstructor, "~> 1.0"},
      {:ok,            "~> 1.0"},
      {:ex_machina,    github: "hippware/ex_machina",     branch: "working"},
      {:faker,         "~> 0.7"},
      {:schemata,      github: "hippware/schemata",       branch: "master"},
      {:ossp_uuid,     github: "hippware/erlang-ossp-uuid", tag: "v1.0.1", manager: :rebar3},
      {:z_stdlib,      github: "zotonic/z_stdlib",        ref: "b9f19b9"},
      {:ejabberd,      github: "hippware/mim-ejabberd",   branch: "working"},
      {:logger_lager_backend, "~> 0.0.2"},
      {:honeybadger,   "~> 0.1"},

      ## ejabberd dependencies
      {:redo,          "~> 2.0", override: true},
      {:cowboy,        "~> 1.0", override: true},
      {:folsom,        "~> 0.8.3", override: true},
      {:idna,          "~> 2.0", override: true},
      {:p1_utils,      "~> 1.0", override: true},
      {:cache_tab,     "~> 1.0", override: true},
      {:stringprep,    "~> 1.0", override: true, manager: :rebar},
      {:base16,        "~> 1.0", override: true},
      {:protobuffs,    "~> 0.8.2", override: true},
      {:erlware_commons, "~> 0.21.0", override: true},
      {:cuesport,      github: "esl/cuesport",            branch: "master", override: true},
      {:exml,          github: "esl/exml",                tag: "2.2.0", override: true},
      {:exometer_core, github: "Feuerlabs/exometer_core", branch: "master", override: true},
      {:exometer,      github: "Feuerlabs/exometer",      branch: "master", manager: :rebar3, override: true},
      {:mochijson2,    github: "bjnortier/mochijson2",    branch: "master", override: true},
      {:alarms,        github: "hippware/alarms",         branch: "master", override: true},
      {:fusco,         github: "esl/fusco",               branch: "master", override: true},
      {:pa,            github: "lavrin/pa",               branch: "master", manager: :rebar3, override: true},
      {:usec,          github: "esl/usec",                branch: "master", override: true},
      {:riak_pb,       github: "basho/riak_pb",           tag: "2.1.4.0", override: true},
      {:riakc,         github: "basho/riak-erlang-client", tag: "2.1.2", manager: :rebar3, override: true},
      {:mustache,      github: "mojombo/mustache.erl",    ref: "d0246fe", override: true},

      ## runtime dependencies (included in release, not needed to build)
      {:recon,         "~> 2.3", override: true},
      {:eper,          "~> 0.94.0"},
      {:binpp,         "~> 1.1"},
      {:pretty_errors, github: "eproxus/pretty_errors",   branch: "master", manager: :rebar},

      ## build dependencies (not included in release)
      {:distillery,    "~> 0.10.1"},
      {:edown,         "~> 0.8.1", override: true},
      # erlando's app file is b0rked so we need to override the dep here.
      {:erlando, ~r//, github: "rabbitmq/erlando",        branch: "master", override: true},
      {:fun_chain,     github: "sasa1977/fun_chain",      branch: "master", manager: :rebar3},

      ## testing dependencies (not included in release)
      {:meck,          "~> 0.8.4", override: true},
      {:espec,         "~> 1.0", only: :test},
      {:dogma,         "~> 0.1.7", only: :dev},
      {:credo,         "~> 0.4.11", only: :dev},
      {:ex_guard,      "~> 1.1", only: :dev},
      {:reprise,       "~> 0.5.0", only: :dev},
      {:dialyxir,      github: "jeremyjh/dialyxir",       branch: "develop", only: :dev},
      {:mix_elvis,     github: "hippware/mix_elvis",      branch: "master", only: :dev},
      {:mix_eunit,     github: "hippware/mix_eunit",      branch: "working", only: :test},
      {:mix_ct,        github: "hippware/mix_ct",         branch: "master", only: :test},
      {:proper,        github: "manopapad/proper",        tag: "v1.2", override: true},
      {:hamcrest,      github: "hyperthunk/hamcrest-erlang", branch: "master", override: true},
      {:escalus,       github: "hippware/escalus",        branch: "working", override: true, only: :test},
      {:exref,         github: "hippware/exref",          branch: "master"}
    ]
  end

  defp aliases do
    [
      deps: ["deps.get", "deps.compile goldrush lager", "compile"],
      lint: "elvis",
      'db.dump.test': "db.dump",
      'db.load.test': "db.load",
      'db.reset.test': "db.reset",
      'db.migrate.test': "db.migrate",
      'db.rollback.test': "db.rollback"
    ]
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
          # {:elvis_style, :macro_names},
          # {:elvis_style, :macro_module_names},
          {:elvis_style, :operator_spaces,
           %{rules: [right: ",", right: "++", left: "++"]}},
          {:elvis_style, :nesting_level, %{level: 3}},
          {:elvis_style, :god_modules, %{limit: 25, ignore: [:wocky_db,
                                                             :wocky_db_bot]}},
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
          {:elvis_style, :macro_names},
          {:elvis_style, :macro_module_names},
          {:elvis_style, :operator_spaces,
           %{rules: [right: ",", right: "++", left: "++"]}},
          {:elvis_style, :nesting_level, %{level: 3}},
          {:elvis_style, :god_modules, %{limit: 25, ignore: [:mam_SUITE,
                                                             :test_helper]}},
          {:elvis_style, :no_if_expression},
          {:elvis_style, :invalid_dynamic_call},
          # Binary patterns in Eunit tests sometimes trip this warning
          # {:elvis_style, :used_ignored_variable},
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
           %{regex: "^(_?[A-Z][0-9a-zA-Z]*)$"}}
        ]}
    ]
  end
end
