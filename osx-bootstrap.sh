#!/bin/sh

echo "** Installing dev dependencies via Homebrew..."
if [ ! -x /usr/local/bin/brew ]; then
    echo "** Installing Homebrew..."
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    brew tap Homebrew/bundle
fi

echo "** Running 'brew bundle'..."
brew bundle

echo "** Starting Cassandra..."
launchctl load ~/Library/LaunchAgents/homebrew.mxcl.cassandra.plist > /dev/null 2>&1

echo "** Loading the dev schema..."
cqlsh -k wocky_shared -e 'EXIT' || cqlsh -f apps/wocky/priv/dev-bootstrap.cql

echo "** done."
