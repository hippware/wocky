node {
  stage 'Prepare'
  sh "epmd -daemon"
  checkout scm
  sh "git submodule update --init"
  sh "make cleanall"
  sh "make compile"

  stage 'Basic Checks'
  sh "make lint"
  sh "make xref"
  sh "make dialyzer"

  stage 'Unit Tests'
  sh "make eunit"

  stage 'Integration Tests'
  sh "make ct"

  stage 'Build Release'
  sh "make tar"
  sh "echo `./version` > RELEASE"

  archive 'RELEASE'
  archive '_build/default/rel/wocky.tar.gz'
}
