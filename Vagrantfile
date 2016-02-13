# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure(2) do |config|
  config.vm.define "wocky-dev", primary: true do |dev|
    # Using Ubuntu 15.10 until the next LTS release comes out
    dev.vm.box = "ubuntu/wily64"

    # Forward the XMPP port to the host
    dev.vm.network "forwarded_port", guest: 5222, host: 5222, auto_correct: true

    # Install Erlang from ESL and Cassandra from Datastax
    dev.vm.provision "shell", inline: <<-SHELL
      if [ ! -x /usr/bin/erl ]; then
        echo 'deb http://packages.erlang-solutions.com/debian wily contrib' > /etc/apt/sources.list.d/esl-erlang.list \
        && curl -L  http://packages.erlang-solutions.com/debian/erlang_solutions.asc | sudo apt-key add - \
        && apt-get update \
        && apt-get install -y libexpat1 libexpat1-dev libssl-dev \
        && apt-get install -y esl-erlang
      fi

      if [ ! -x /usr/sbin/cassandra ]; then
        echo 'deb http://debian.datastax.com/community stable main' > /etc/apt/sources.list.d/cassandra.list \
        && curl -L https://debian.datastax.com/debian/repo_key | sudo apt-key add - \
        && apt-get update \
        && apt-get install -y openjdk-8-jre-headless \
        && apt-get install -y cassandra cassandra-tools dsc30
      fi

      /vagrant/rebar3 update
    SHELL

    # Since we are running Cassandra in the VM we need to allocate more
    # RAM and another VCPU or Cassandra won't start due to OOM errors
    dev.vm.provider "virtualbox" do |vbox|
      vbox.gui = false
      vbox.memory = 2048
      vbox.cpus = 2
    end
  end

  config.vm.define "wocky-build", primary: true do |build|
    build.vm.box = "ubuntu/trusty64"
    build.vm.provision "shell", inline: <<-SHELL
      if [ ! -x /usr/bin/erl ]; then
        echo 'deb http://packages.erlang-solutions.com/debian trusty contrib' > /etc/apt/sources.list.d/esl-erlang.list \
        && curl -L  http://packages.erlang-solutions.com/debian/erlang_solutions.asc | sudo apt-key add - \
        && apt-get update \
        && apt-get install -y git g++ libexpat1 libexpat1-dev libssl-dev \
        && apt-get install -y esl-erlang
      fi
      /vagrant/rebar3 update
    SHELL
    build.vm.provider "virtualbox" do |vbox|
      vbox.gui = false
      vbox.memory = 1024
      vbox.cpus = 1
    end
  end
end
