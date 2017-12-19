Wocky
========
This is the main server-side component of the TinyRobot application. It
incorporates an XMPP server which has been extended with custom functionality
and an HTTP API. The application is built in a Docker container and runs on a
Kubernetes cluster.

Building
========
Use `git` to checkout the code:

    $ git clone git@github.com:hippware/wocky.git

The project uses both Elixir and Erlang and is built using the `mix` tool.
After the first checkout, use `mix prepare` to download and compile all
dependencies.

Database schema
===============
Wocky uses both PostgreSQL with PostGIS and Redis.

You can drop the old database (if any), create the database schema, and run the
migrations with:

    $ mix ecto.reset

Testing
=======
Wocky uses Espec and ExUnit for unit tests. The unit test suites can be run
with `mix spec` and `mix test`, respectively. Integration tests use Erlang's
Common Test library and are run using `mix ct`.

Releases
========
The final release artifact for Wocky is a Docker container. The container is
built by the CI system (Codeship) whenever changes are published to `master`.
It is possible to manually build a release artifact using `make build` followed
by `make push`, but this is rarely necessary.

The release image is pushed to an Amazon ECR repository and is tagged with the
Git SHA of the commit that was built. There is a user-friendly version number,
but the deploymement system uses commit SHAs exclusively to identify specific
releases.

There are 4 production environments to which the code can be deployed:

1. `load`: Used to load test the application
2. `testing`: Used for running front-end integration tests
3. `staging`: Used for testing production-ready code
4. `us1`: The first (and currently only) customer-facing environment

Deploying
=========
In order to deploy you need `kubernetes-deploy` and `ejson`. These both require
Ruby version 2.3 or greater. Use `gem` to install them:

    $ gem install kubernetes-deploy ejson

You may have to use `sudo`, depending on how your system is setup.

Deployment and general cluster ops are automated using `make`. To deploy, use
`make deploy` and specify the target environment by setting `WOCKY_ENV`. For
example, to deploy to `staging`, use:

    $ make deploy WOCKY_ENV=staging

By default, the release corresponding to the current Git SHA is deployed. If
there is no release corresponding to the current Git SHA, you will get a
confusing error message. If you want to specify which release is deployed,
you can set `IMAGE_TAG`:

    $ make deploy WOCKY_ENV=staging IMAGE_TAG=6eb2a571f9ad00407a6c5bef77ea9011e92bb9ca

Of course, this only works if there is a release tagged with that SHA. It is
not a good idea to try and deploy `latest`. It may or may not do what you want.

The deployment process performs the following steps:

1. Verifies that all of the required resources are present
2. Pushes any new or updated secrets to the cluster
3. Synchronizes any ConfigMaps to the cluster
4. Runs the "predeploy" pod to notify Slack and run database migrations
5. Updates any other resources (services, etc)
6. Updates the Wocky Deployment with the image tag and kicks off a rollout
7. Waits for the rollout to complete

If you want to edit the secrets in the `secrets.ejson` file, you will need the
keys. The keys are stored in a CSV file in LastPass. The `ejson` command looks
for keys in the directory `/opt/ejson/keys/`. Each key should be a file named
after the public key and containing the private key. Create the files with a
command like:

    $ echo {private_key} > /opt/ejson/keys/{public_key}
