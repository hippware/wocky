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

To build, once the repository is checked out type

    $ make
    $ make rel

(The first `make` is optional but shown for completeness.)

This will build wocky into rel/wocky. Once the release is built, start the
server using

    $ cd rel/wocky
    $ bin/wocky live

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

