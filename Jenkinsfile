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
        }
      }
    }

    stage('Basic Checks') {
      environment {
        MIX_ENV = "test"
      }

      steps {
        ansiColor('xterm') {
          sh "mix prepare"
          sh "mix ecto.reset"
          sh "mix lint"
          sh "mix dialyzer --halt-exit-status"
        }
      }
    }

    stage('Unit Tests') {
      environment {
        MIX_ENV = "test"
      }

      steps {
        ansiColor('xterm') {
          sh "mix test"
          sh "mix espec"
        }
      }
    }

    stage('Integration Tests') {
      environment {
        MIX_ENV = "test"
      }

      steps {
        ansiColor('xterm') {
          sh "mix ct"
        }
      }
    }

    stage('Build Release') {
      environment {
        MIX_ENV = "prod"
      }

      steps {
        ansiColor('xterm') {
          sh "rm -rf _build/prod/rel/wocky"
          sh "mix prepare"
          sh "mix release --warnings-as-errors"
          sh "elixir version.exs > RELEASE"

          archive 'RELEASE'
          archive '_build/prod/rel/wocky/releases/**/wocky.tar.gz'
        }
      }
    }
  }
}

