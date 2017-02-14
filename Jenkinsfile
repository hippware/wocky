node {
  wrap([$class: 'AnsiColorBuildWrapper']) {
    stage('Prepare') {
      sh "epmd -daemon"
      checkout scm
      sh "mix local.hex --force"
      sh "mix local.rebar --force"
      sh "mix clean"
      sh "mix prepare"
    }

    stage('Basic Checks') {
      sh "mix lint"
      sh "mix exref"
    }

    stage('Unit Tests') {
      env.CQLSH_NO_BUNDLED = "true"
      sh "mix db.test_migrations"
      sh "mix db.load.test"
      sh "MIX_ENV=test mix prepare"
      sh "mix espec"
      sh "mix eunit"
      sh "mix dialyzer --halt-exit-status"
    }

    stage('Integration Tests') {
      sh "mix ct"
    }

    stage('Build Release') {
      sh "rm -rf _build/prod/rel/wocky"
      sh "MIX_ENV=prod mix prepare"
      sh "MIX_ENV=prod mix release --warnings-as-errors"
      sh "echo `./version` > RELEASE"

      archive 'RELEASE'
      archive '_build/prod/rel/wocky/releases/**/wocky.tar.gz'
    }
  }
}
