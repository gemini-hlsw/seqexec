#!/bin/bash
set -xe

###
### BUILD
###

/usr/local/bin/sbt             \
  -jvm-opts travis-jvmopts     \
  -no-colors                   \
  -Docs3.skipDependencyUpdates \
  headerCheck                  \
  test:headerCheck             \
  scalastyle                   \
  compile

###
### TEST
###

# Start a new Postgres container for this build
# TODO: read postgres version from the build
CID=`docker run --detach --publish 5432 postgres:9.6.0`

# Add an exit handler to ensure we always clean up.
function cleanup {
  docker stop $CID
  docker rm --volumes --force $CID
}
trap cleanup EXIT

# Get our host and port ... like 0.0.0.0:32751
HOST_AND_PORT=`docker port $CID 5432/tcp`

# The postgres user already exists, so we can go ahead and create the database
until docker exec $CID psql -U postgres -c 'create database gem'
do
  echo "waiting for postgres container..."
  sleep 0.5
done

# Set up the schema and run tests
/usr/local/bin/sbt                                        \
  -jvm-opts travis-jvmopts                                \
  -no-colors                                              \
  -Docs3.skipDependencyUpdates                            \
  -Docs3.databaseUrl=jdbc:postgresql://$HOST_AND_PORT/gem \
  sql/flywayMigrate                                       \
  test

###
### JAVASCRIPT PACKAGING
###

/usr/local/bin/sbt                      \
  -jvm-opts travis-jvmopts              \
  -no-colors                            \
  -Docs3.skipDependencyUpdates          \
  ui/fastOptJS                          \
  seqexec_web_client/fastOptJS::webpack
