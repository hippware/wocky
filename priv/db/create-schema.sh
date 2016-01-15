#!/bin/sh

pp() {
    echo "==> $1"
}

pp "Creating dev schema in Cassandra..."

SCRIPT=$(readlink $0 || true)
if [ -z $SCRIPT ]; then
    SCRIPT=$0
fi;
DB_DIR="$(cd `dirname "$SCRIPT"` && pwd -P)"
pp "Using scripts in ${DB_DIR}..."

CQLSH=`which cqlsh`
pp "Using cqlsh '${CQLSH}'..."

run_cql() {
    SCRIPT_NAME=$1.cql
    SCRIPT_FILE=$DB_DIR/$SCRIPT_NAME
    KEYSPACE=$2

    KEYSPACE_ARG=""
    if [ "$KEYSPACE" != "" ]; then
        KEYSPACE_ARG="-k $2"
        pp "Loading $SCRIPT_NAME into keyspace $KEYSPACE..."
    else
        pp "Loading $SCRIPT_NAME..."
    fi

    $CQLSH $KEYSPACE_ARG -f $SCRIPT_FILE 2>&1 | grep -v AlreadyExists
}

# Creates the following keyspaces:
# wocky_shared, wocky_test_shared, wocky_localhost and wocky_test_localhost
run_cql dev-keyspaces

# Load tables into the proper keyspaces
run_cql shared-tables wocky_shared
run_cql local-tables wocky_localhost

run_cql shared-tables wocky_test_shared
run_cql local-tables wocky_test_localhost

pp "Done."
