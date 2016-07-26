defmodule Wocky.Mixfile do
  use Mix.Project

  def project do
    [app: :wocky,
     version: version,
     compilers: [:erlang, :app],
     language: :erlang,
     erlc_options: [
       :debug_info,
       :warnings_as_errors,
       # :warn_export_all,
       :warn_export_vars,
       :warn_obsolete_guard,
       :warn_unused_import,
       {:warn_format, 1},
       {:parse_transform, :lager_transform}
     ],
     test_coverage: [output: "_build/#{Mix.env}/cover"],
     aliases: aliases,
     deps: deps,
     preferred_cli_env: [eunit: :test,
                         ct:    :test],
     dialyzer: [
       plt_apps: [
         :compiler, :crypto, :erts, :kernel, :stdlib, :mnesia, :ssl, :ssh,
         :xmerl, :public_key, :tools, :sasl, :hipe, :edoc, :syntax_tools,
         :runtime_tools, :inets, :asn1, :cowboy, :cowlib, :exml, :p1_utils,
         :binpp, :pretty_errors, :mochijson2, :erlando, :z_stdlib, :uuid
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

  def application do
    [description: 'JabberWocky XMPP Server',
     applications: [
       :kernel,
       :stdlib,
       :crypto,
       :lager,
       :ossp_uuid,
       :cqerl,
       :stringprep,
       :z_stdlib,
       :mochijson2,
       :qdate,
       :ssl,
       :ibrowse,
       :jiffy,
       :algolia
     ],
     included_applications: [:ejabberd],
     mod: {:wocky_app, []},
     env: [
       {:wocky_env, 'dev'},
       {:config_dir, 'etc'},
       {:francus_chunk_size, 1048576}, # 1MB
       {:cassandra_nodes, [{'127.0.0.1', 9042}]},
       {:cassandra_opts, []},
       {:keyspace_prefix, 'wocky_test_'},
       {:keyspace_replication, {:simple, 1}},
       {:indexing_enabled_envs, ['staging']},
       {:algolia_app_id, 'HIE75ZR7Q7'},
       {:algolia_app_key, '79602842342e137c97ce188013131a89'},
       {:algolia_index_name, 'dev_wocky_users'}
     ]]
  end

  defp deps do
    [
      {:pooler,        "1.5.0", override: true},
      {:setup,         "1.7.0", override: true},
      {:jiffy,         "0.14.7", override: true},
      {:lager,         "3.2.1", override: true},
      {:cqerl,         github: "hippware/cqerl",          branch: "working", override: true},
      {:schemata,      github: "toland/schemata",         branch: "master"},
      {:ossp_uuid,     github: "hippware/erlang-ossp-uuid", tag: "v1.0.1", manager: :rebar3},
      {:qdate,         github: "choptastic/qdate",        ref: "10d56c2"},
      {:z_stdlib,      github: "zotonic/z_stdlib",        ref: "b9f19b9"},
      {:algolia,       github: "k3nn7/algoliasearch-client-erlang", branch: "master"},
      {:ejabberd,      github: "hippware/mim-ejabberd",   branch: "master"},

      ## ejabberd dependencies
      {:redo,          "2.0.1", override: true},
      {:cowboy,        "1.0.4", override: true},
      {:folsom,        "0.8.3", override: true},
      {:idna,          "2.0.0", override: true},
      {:p1_utils,      "1.0.4", override: true},
      {:cache_tab,     "1.0.2", override: true},
      {:stringprep,    "1.0.3", override: true, manager: :rebar},
      {:base16,        "1.0.0", override: true},
      {:protobuffs,    "0.8.2", override: true},
      {:erlware_commons, "0.21.0", override: true},
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
      {:recon,         "2.3.1", override: true},
      {:eper,          "0.94.0"},
      {:binpp,         github: "jtendo/binpp",            branch: "master"},
      {:pretty_errors, github: "eproxus/pretty_errors",   branch: "master", manager: :rebar},

      ## build dependencies (not included in release)
      {:edown,         "0.8.1", override: true},
      {:erlando,       github: "rabbitmq/erlando",        branch: "master"},
      {:fun_chain,     github: "sasa1977/fun_chain",      branch: "master", manager: :rebar3},

      ## testing dependencies (not included in release)
      {:meck,          "0.8.4", override: true},
      {:espec,         "~> 0.8.22", only: :test},
      {:dialyxir,      "~> 0.3.5", only: :dev},
      {:mix_elvis,     github: "hippware/mix_elvis",      branch: "master", only: :dev},
      {:mix_eunit,     github: "hippware/mix_eunit",      branch: "working", only: :test},
      {:mix_ct,        github: "hippware/mix_ct",         branch: "master", only: :test},
      {:proper,        github: "manopapad/proper",        tag: "v1.2", override: true},
      {:hamcrest,      github: "hyperthunk/hamcrest-erlang", branch: "master", override: true},
      {:escalus,       github: "hippware/escalus",        branch: "working", override: true, only: :test}
    ]
  end

  defp aliases do
    [deps: ["deps.get", "deps.compile goldrush lager", "compile"]]
  end

  defp elvis_config do
    [
      %{dirs: ['src', 'test'],
        filter: '*.erl',
        rules: [
          {:elvis_style, :line_length,
           %{ignore: [], limit: 80, skip_comments: false}},
          {:elvis_style, :no_tabs},
          {:elvis_style, :no_trailing_whitespace},
          {:elvis_style, :macro_names, %{ignore: []}},
          {:elvis_style, :macro_module_names},
          {:elvis_style, :operator_spaces,
           %{rules: [right: ",", right: "++", left: "++"]}},
          {:elvis_style, :nesting_level, %{level: 3}},
          {:elvis_style, :god_modules, %{limit: 25, ignore: []}},
          {:elvis_style, :no_if_expression},
          {:elvis_style, :invalid_dynamic_call, %{ignore: [:mod_wocky_roster]}},
          {:elvis_style, :used_ignored_variable},
          {:elvis_style, :no_behavior_info},
          {:elvis_style, :module_naming_convention,
           %{regex: "^[a-z]([a-z0-9]*_?)*(_SUITE)?$", ignore: []}},
          {:elvis_style, :function_naming_convention,
           %{regex: "^([a-z][a-z0-9]*_?)*|'>>='$"}},
          {:elvis_style, :state_record_and_type},
          {:elvis_style, :no_spec_with_records},
          {:elvis_style, :dont_repeat_yourself, %{min_complexity: 15}},
          {:elvis_style, :no_debug_call, %{ignore: [:mam_SUITE]}},
          {:elvis_style, :variable_naming_convention,
           %{regex: "^(_?[A-Z][0-9a-zA-Z]*)$"}}
        ]}
    ]
  end
end
