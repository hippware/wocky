## -*- mode: ruby -*-
## ex: ft=ruby

# This file will help new developers get up to speed quickly on a Mac. Using
# Homebrew (http://brew.sh) new developers can install the software necessary
# to get started. Install Homebrew according to the instructions on the site,
# then "tap" the bundle cask (only needs to be done once):
#
# $ brew tap Homebrew/bundle
#
# Now you can run `brew bundle` from the root directory of the wocky project
# and Homebrew will install everything you need.

cask_args appdir: '/Applications'
tap 'caskroom/cask'

# Our core tech stack
brew 'cassandra'
brew 'erlang'
brew 'git'

# ejabberd dependencies
brew 'openssl'
brew 'protobuf'
brew 'unixodbc'

# While testing ejabberd, these may come in handy, but aren't required
#brew 'mysql'
#brew 'redis'

# CLI tools for services we use
brew 'awscli'
brew 'lastpass-cli'

# Vagrant and dependencies
cask 'virtualbox'
cask 'vagrant'
