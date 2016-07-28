node {
  stage 'Prepare'
  sh "epmd -daemon"
  checkout scm
  sh "mix local.hex --force"
  sh "mix deps"

  stage 'Basic Checks'
  sh "mix lint"
  sh "mix xref warnings"
  sh "mix dialyzer"

  stage 'Unit Tests'
  sh "mix eunit"

  stage 'Integration Tests'
  sh "mix ct"

  stage 'Build Release'
  sh "rm -rf rel/wocky"
  sh "mix release"
  sh "echo `./version` > RELEASE"

  archive 'RELEASE'
  archive 'rel/wocky/releases/**/wocky.tar.gz'
}
