Wocky
========
This is the main server-side software. At the moment, it incorporates an XMPP
server which will be extended with custom functionality.

Building
========
For information about prerequisites, please refer to the MongooseIM
documentation.

In order the checkout the code and initialize the Git submodules, pass the
`--recursive` flag to `git clone`:

    $ git clone --recursive git@github.com:hippware/wocky.git

Alternatively, if the repository is already checked out, you can use the
following commands:

    $ git submodule init
    $ git submodule update

Then, the rebar3 package index needs to be initialized by typing:

    $ ./rebar3 update

This tells rebar3 how to fetch rebar3 plugins. It only needs to be done once.

To build, once the repository is checked out type

    $ ./rebar3 release

This will download all of the dependencies, compile everything and create a
release in _build/default/rel/wocky. Once the release is built, start the
server using:

    $ cd _build/default/rel/wocky
    $ bin/wocky live

To create a production release, use:

    $ ./rebar3 as prod release

The production release will be built in _build/prod/rel/wocky. If you want a
tarball of the production release use the command:

    $ ./rebar3 as prod tar

Running integration tests
=========================

To run the ejabberd integration tests, you need to build and start two test
nodes:

    $ make testrel

This creates two new test nodes in the directory _build/test\_nodeN/rel/wocky. For each node,
cd into the node directory and start it:

    $ cd _build/test_node1/rel/wocky
    $ bin/wocky live

Once both nodes are started, run the tests with:

    $ make quicktest

Module naming convention
========================

To help keep everything straight we are following a simple naming convention.

Erlang modules that:
* interact with the database are prefixed with 'wocky_db'
* implement functionality unique to wocky as ejabberd modules are prefixed with
'mod_wocky'
* implement backend functionality for existing ejabberd modules follow the naming
convention established by ejabberd with 'wocky' as the backend name (i.e.,
'ejabberd\_auth\_wocky' or 'mod\_roster\_wocky')

Repository separation
=====================
Wocky incorporates an open source XMPP server. It also contains fully
proprietary custom functionality. In order to separate public intellectual
property from private intellectual property, the source code needs to be
partitioned into distinct repositories.

This repository contains fully proprietary code and features.

Wocky is the "top level" project and the XMPP server is a component sub-project.

In the development environment, this is the "parent" git repository and the
XMPP server is a nested git repository located in the `ext/` directory (managed
by `git` as a submodule). In other words, the development tree has (at least)
two git repositories. This is important to keep in mind.

Wocky is the "top level" Erlang release and the XMPP server (along with other
components) is a dependency.

Nesting of git repositories is accomplished by telling git to ignore the deps/
directory (ie. `deps/` is listed in .gitignore). This method may not be the best
nor standard way of nesting git repositories but it is the most natural fit to
the directory structure used by Erlang/OTP and various build tools (ie. rebar).

This repository will contain all of the custom functionality and features which
extend the vanilla XMPP server.

