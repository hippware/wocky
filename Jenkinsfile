node {
  try {

    mail (
        bcc: '',
        body: "Build ID failed for ${JOB_NAME}. See output at ${JOB_URL}",
        cc: '',
        from: 'noreply@jenkins.dev.tinyrobot.com',
        replyTo: '',
        to: "${CHANGE_AUTHOR_EMAIL}",
        subject: "Jenkins build FAILED for ${JOB_NAME} on ${BRANCH_NAME}"
    )

    wrap([$class: 'AnsiColorBuildWrapper']) {
      stage('Prepare') {
        sh "epmd -daemon"
        checkout scm
        sh "mix local.hex --force"
        sh "mix local.rebar --force"
        sh "mix clean"
        sh "mix deps"
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
  } catch (e) {
    mail (
      subject: 'Jenkins build FAILED for $JOB_NAME on $BRANCH_NAME',
      from: 'noreply@jenkins.dev.tinyrobot.com',
      to: '$CHANGE_AUTHOR_EMAIL',
      body: 'Build ID $BUILD_ID failed. See output at $JOB_URL'
    )
    throw e
  }
}
