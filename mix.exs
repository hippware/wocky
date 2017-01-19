defmodule Wocky.Mixfile do
  use Mix.Project

  def project do
    [app: :wocky,
     version: version(),
     elixir: "~> 1.4",
     # If this is set to true, then our ejabberd dependency won't build
     # properly in the prod environment.
     build_embedded: false, # Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     erlc_options: erlc_options(Mix.env),
     test_coverage: [output: "_build/#{Mix.env}/cover"],
     aliases: aliases(),
     deps: deps(),
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
         :binpp, :mochijson2, :erlando, :z_stdlib, :uuid, :cqerl, :riakc,
         :erlang_murmurhash, :timex, :ejabberd, :lager, :ossp_uuid, :algolia,
         :logger, :schemata, :porcelain, :geocalc, :mix, :faker, :ex_machina,
         :base16, :poison, :ex_aws, :exconstructor, :honeybadger
       ],
       plt_add_deps: true,
       flags: [
         "--fullpath", "-Wunmatched_returns", "-Werror_handling",
         "-Wrace_conditions", "-Wunderspecs", "-Wunknown"
       ]
     ],
     elvis_config: elvis_config(),
     # set switches that affect every invocation of the eunit task
     eunit: [
       start: true
     ]
   ]
  end

  defp version do
    {ver_result, _} = System.cmd("bash", ["./version"])
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

  def application do
    [description: 'JabberWocky XMPP Server',
     extra_applications: [
       :crypto, :ssl, :logger, :plug, :runtime_tools, :cowboy, :poison
     ],
     included_applications: [
       # These are here because we start them manually and do not want them
       # starting automatically when Wocky starts.
       :schemata, :ejabberd,

       # ejabberd dependencies that aren't listed in ejabberd.app
       # Some of these aren't used with our configuration, so we don't want
       # them to automatically start. We assume that ejabberd will start them
       # if necessary.
       :alarms, :cache_tab, :cuesport, :fusco, :jiffy, :lager_syslog,
       :p1_utils, :pa, :poolboy, :recon, :redo, :riakc, :usec, :xmerl
     ],
     mod: {:wocky_app, []},
     env: [
       wocky_env: 'dev',
       config_dir: 'etc',
       francus_chunk_size: 1048576, # 1MB
       keyspace_prefix: 'wocky_test_',
       location_api_port: 8080,
       enable_follow_me: false,
       algolia_user_index_name: [
         staging: 'dev_wocky_users',
         us1:     'prod_wocky_users'
       ],
       algolia_bot_index_name: [
         staging: 'dev_wocky_bots',
         us1:     'prod_wocky_bots'
       ],
       notification_enabled_envs: ['staging', 'us1'],
       notification_handler: Wocky.Notification.NullHandler,
       reserved_handles: [
         "root",
         "admin",
         "super",
         "superuser",
         "tinyrobot",
         "hippware",
         "www",
         "support",
         "null"
       ]
     ]]
  end

  defp deps do
    [
      {:lager,                "~> 3.2",   override: true},
      {:meck,                 "~> 0.8.4", override: true, runtime: false},
      {:hackney,              "~> 1.6",   override: true},
      {:base16,               "~> 1.0",   override: true},
      {:porcelain,            "~> 2.0"},
      {:algolia,              "~> 0.4.0"},
      {:ex_aws,               "~> 1.0"},
      {:geocalc,              "~> 0.5.3"},
      {:exconstructor,        "~> 1.0"},
      {:ok,                   "~> 1.2",    runtime: false},
      {:exactor,              "~> 2.2",    runtime: false},
      {:faker,                "~> 0.7.0"},
      {:honeybadger,          "~> 0.6"},
      {:logger_lager_backend, "~> 0.0.2"},
      {:distillery,           "~> 1.1",    runtime: false},
      {:eper,                 "~> 0.94.0"},
      {:binpp,                "~> 1.1"},
      {:espec,                "~> 1.2",    only: :test},
      {:dogma,                "~> 0.1.13", only: :dev, runtime: false},
      {:credo,                "~> 0.5.3",  only: :dev, runtime: false},
      {:ex_guard,             "~> 1.1",    only: :dev, runtime: false},
      {:reprise,              "~> 0.5.0",  only: :dev},

      {:z_stdlib,   github: "zotonic/z_stdlib",      ref: "b9f19b9"},
      {:ejabberd,   github: "hippware/mim-ejabberd", branch: "working-test"},
      {:schemata,   github: "hippware/schemata",     branch: "master"},
      {:ex_machina, github: "thoughtbot/ex_machina", branch: "master"},
      {:ossp_uuid,
        github: "hippware/erlang-ossp-uuid",
        tag: "v1.0.1",
        manager: :rebar3},
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
      {:dialyxir,
        github: "jeremyjh/dialyxir",
        branch: "develop",
        runtime: false,
        only: :dev},
      {:mix_elvis,
        github: "hippware/mix_elvis",
        branch: "master",
        runtime: false,
        only: :dev},
      {:mix_eunit,
        github: "hippware/mix_eunit",
        branch: "working",
        only: :test},
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

      # Overrides
      # These are transitive dependencies that need to be overriden to build
      # correctly. They are not used directly by Wocky.
      {:uuid,   "~> 1.6.0", override: true, hex: :uuid_erl},
      {:edown,  "~> 0.8.1", override: true, runtime: false},
      {:folsom, "~> 0.8.3", override: true},
      {:syslog,
        github: "Vagabond/erlang-syslog",
        branch: "master",
        override: true,
        manager: :rebar3},
      {:riak_pb,
        github: "basho/riak_pb",
        tag: "2.2.0.2",
        override: true,
        runtime: false,
        manager: :rebar},
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
      prepare: ["deps.get", "deps.compile goldrush lager", "compile"],
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
           %{regex: "^(_?_?[A-Z][0-9a-zA-Z]*)$"}}
        ]}
    ]
  end
end
