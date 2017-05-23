pipeline {
  agent any

  environment {
    WOCKY_DB_HOST = 'wocky-jenkins-test.cfzxbikdyqbd.us-east-1.rds.amazonaws.com'
  }

  stages {
    stage('Prepare') {
      steps {
        ansiColor('xterm') {
          sh "epmd -daemon"
          checkout scm
          sh "mix local.hex --force"
          sh "mix local.rebar --force"
          sh "mix clean"
          sh "mix prepare"
        }
      }
    }

    stage('Basic Checks') {
      steps {
        ansiColor('xterm') {
          sh "mix lint"
          sh "mix dialyzer --halt-exit-status"
        }
      }
    }

    stage('Unit Tests') {
      steps {
        ansiColor('xterm') {
          sh "MIX_ENV=test mix prepare"
          sh "MIX_ENV=test mix ecto.drop"
          sh "MIX_ENV=test mix ecto.create"
          sh "MIX_ENV=test mix ecto.migrate"
          sh "mix espec"
        }
      }
    }

    stage('Integration Tests') {
      steps {
        ansiColor('xterm') {
          sh "mix ct"
        }
      }
    }

    stage('Build Release') {
      steps {
        ansiColor('xterm') {
          sh "rm -rf _build/prod/rel/wocky"
          sh "MIX_ENV=prod mix prepare"
          sh "MIX_ENV=prod mix release --warnings-as-errors"
          sh "echo `./version` > RELEASE"

          archive 'RELEASE'
          archive '_build/prod/rel/wocky/releases/**/wocky.tar.gz'
        }
      }
    }
  }
}