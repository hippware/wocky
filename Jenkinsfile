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
      sh "mix dialyzer"
    }

    stage('Unit Tests') {
      env.CQLSH_NO_BUNDLED = "true"
      sh "mix db.test_migrations"
      sh "mix db.load.test"
      sh "mix espec"
      sh "mix eunit"
    }

    stage('Integration Tests') {
      sh "mix ct"
    }

    stage('Build Release') {
      sh "rm -rf rel/wocky"
      sh "MIX_ENV=prod mix release --warnings-as-errors"
      sh "echo `./version` > RELEASE"

      archive 'RELEASE'
      archive 'rel/wocky/releases/**/wocky.tar.gz'
    }
  }
}
