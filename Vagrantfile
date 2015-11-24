# -*- mode: ruby -*-
# vi: set ft=ruby :

# All Vagrant configuration is done below. The "2" in Vagrant.configure
# configures the configuration version (we support older styles for
# backwards compatibility). Please don't change it unless you know what
# you're doing.
Vagrant.configure(2) do |config|
  # The most common configuration options are documented and commented below.
  # For a complete reference, please see the online documentation at
  # https://docs.vagrantup.com.

  config.vm.define "wocky-local", primary: true do |local|
    # Using Ubuntu 15.10 until the next LTS release comes out
    local.vm.box = "ubuntu/wily64"

    # Forward the XMPP port to the host
    local.vm.network "forwarded_port", guest: 5280, host: 5280, auto_correct: true

    # Install Erlang from ESL, Cassandra from Datastax then load the wocky dev
    # schema into Cassandra.
    config.vm.provision "shell", inline: <<-SHELL
      if [ ! -x /usr/bin/erl ]; then
        wget -nv https://packages.erlang-solutions.com/erlang/esl-erlang/FLAVOUR_1_general/esl-erlang_18.1-1~ubuntu~precise_amd64.deb \
        && dpkg -i esl-erlang_18.1-1~ubuntu~precise_amd64.deb \
        && apt-get update \
        && apt-get install -y esl-erlang \
        || apt-get -y -f install
      fi

      if [ ! -x /usr/sbin/cassandra ]; then
        echo 'deb http://debian.datastax.com/community stable main' > /etc/apt/sources.list.d/cassandra.list \
        && curl -L https://debian.datastax.com/debian/repo_key | sudo apt-key add - \
        && apt-get update \
        && apt-get install -y openjdk-8-jre-headless \
        && apt-get install -y cassandra=2.2.3 cassandra-tools=2.2.3 dsc22
      fi

      sleep 5
      cqlsh -k wocky_shared -e 'EXIT' || cqlsh -f /vagrant/apps/wocky/priv/dev-bootstrap.cql
    SHELL

    # Technically we could provision Cassandra using Docker, but I was
    # having problems getting the container to start properly
    # local.vm.provision "docker" do |d|
    #   d.pull_images "cassandra"
    #   d.run 'cassandra',
    #         args: '--name wocky-cassandra -d cassandra:2.2'
    # end

    # Since we are running Cassandra in the VM we need to allocate more
    # RAM and another VCPU or Cassandra won't start due to OOM errors
    local.vm.provider "virtualbox" do |vbox|
      vbox.memory = 2048
      vbox.cpus = 2
    end
  end

  # Every Vagrant development environment requires a box. You can search for
  # boxes at https://atlas.hashicorp.com/search.
  # config.vm.box = "ubuntu/wily64"

  # Disable automatic box update checking. If you disable this, then
  # boxes will only be checked for updates when the user runs
  # `vagrant box outdated`. This is not recommended.
  # config.vm.box_check_update = false

  # Create a forwarded port mapping which allows access to a specific port
  # within the machine from a port on the host machine. In the example below,
  # accessing "localhost:8080" will access port 80 on the guest machine.
  # config.vm.network "forwarded_port", guest: 80, host: 8080

  # Create a private network, which allows host-only access to the machine
  # using a specific IP.
  # config.vm.network "private_network", ip: "192.168.33.10"

  # Create a public network, which generally matched to bridged network.
  # Bridged networks make the machine appear as another physical device on
  # your network.
  # config.vm.network "public_network"

  # Share an additional folder to the guest VM. The first argument is
  # the path on the host to the actual folder. The second argument is
  # the path on the guest to mount the folder. And the optional third
  # argument is a set of non-required options.
  # config.vm.synced_folder "../data", "/vagrant_data"

  # Provider-specific configuration so you can fine-tune various
  # backing providers for Vagrant. These expose provider-specific options.
  # Example for VirtualBox:
  #
  # config.vm.provider "virtualbox" do |vb|
  #   # Display the VirtualBox GUI when booting the machine
  #   vb.gui = true
  #
  #   # Customize the amount of memory on the VM:
  #   vb.memory = "1024"
  # end
  #
  # View the documentation for the provider you are using for more
  # information on available options.

  # Define a Vagrant Push strategy for pushing to Atlas. Other push strategies
  # such as FTP and Heroku are also available. See the documentation at
  # https://docs.vagrantup.com/v2/push/atlas.html for more information.
  # config.push.define "atlas" do |push|
  #   push.app = "YOUR_ATLAS_USERNAME/YOUR_APPLICATION_NAME"
  # end

  # Enable provisioning with a shell script. Additional provisioners such as
  # Puppet, Chef, Ansible, Salt, and Docker are also available. Please see the
  # documentation for more information about their specific syntax and use.
  # config.vm.provision "shell", inline: <<-SHELL
  #   sudo apt-get update
  #   sudo apt-get install -y apache2
  # SHELL
end
