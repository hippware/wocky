# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure(2) do |config|
  # Using Ubuntu 15.10 until the next LTS release comes out
  config.vm.box = "ubuntu/wily64"

  config.vm.define "wocky-local", primary: true do |local|
    # Forward the XMPP port to the host
    local.vm.network "forwarded_port", guest: 5280, host: 5280, auto_correct: true

    # Install Erlang from ESL and Cassandra from Datastax
    local.vm.provision "shell", inline: <<-SHELL
      if [ ! -x /usr/bin/erl ]; then
        echo 'deb http://packages.erlang-solutions.com/debian trusty contrib' > /etc/apt/sources.list.d/esl-erlang.list \
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
    local.vm.provider "virtualbox" do |vbox|
      vbox.gui = false
      vbox.memory = 2048
      vbox.cpus = 2
    end
  end
end
