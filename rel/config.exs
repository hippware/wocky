use Mix.Releases.Config,
    # This sets the default release built by `mix release`
    default_release: :default,
    # This sets the default environment used by `mix release`
    default_environment: :prod

# For a full list of config options for both releases
# and environments, visit https://hexdocs.pm/distillery/configuration.html


# You may define one or more environments in this file,
# an environment's settings will override those of a release
# when building in that environment, this combination of release
# and environment configuration is called a profile

environment :dev do
  set dev_mode: true
  set include_erts: false
  set cookie: :"%dmqKgt{[Wt.Np>}s|,y(F0y!4h`R)nBgsMoO<!=0)p/Sxuj=0o;z*=y`zF1`^o]"
end

environment :prod do
  set include_erts: true
  set include_src: false
  set cookie: :"%dmqKgt{[Wt.Np>}s|,y(F0y!4h`R)nBgsMoO<!=0)p/Sxuj=0o;z*=y`zF1`^o]"
end

# You may define one or more releases in this file.
# If you have not set a default release, or selected one
# when running `mix release`, the first release in the file
# will be used by default

release :wocky do
  set version: current_version(:wocky)
  set applications: [
    :wocky,
    :wocky_api,
    :wocky_xmpp
  ]
  set vm_args: "rel/vm.args"
  set overlays: [
    {:copy, "rel/wockyctl", "bin/wockyctl"},
    {:copy, "etc", "etc"}
  ]
  set commands: [
    "predeploy": "rel/commands/predeploy.sh",
    "weekly": "rel/commands/weekly.sh"
  ]
end

