#!/bin/bash
set -x

# Remove any cruft leftover from the last build.
# This ensures we avoid a confused incremental compiler.
# git clean -xdf

# Start a new Postgres container for this build
# TODO: read postgres version from the build
CID=`docker run --detach --publish 5432 postgres:9.6.0`

# Get our host and port ... like 0.0.0.0:32751
HOST_AND_PORT=`docker port $CID 5432/tcp`

# The postgres user already exists, so we can go ahead and create the database
until docker exec $CID psql -U postgres -c 'create database gem'
do
  echo "waiting for postgres container..."
  sleep 0.5
done

# Ready to do the build!
sbt                                                       \
  -jvm-opts travis-jvmopts                                \
  -no-colors                                              \
  -Docs3.skipDependencyUpdates                            \
  -Docs3.databaseUrl=jdbc:postgresql://$HOST_AND_PORT/gem \
  headerCheck                                             \
  test:headerCheck                                        \
  scalastyle                                              \
  sql/flywayMigrate                                       \
  compile                                                 \
  test                                                    \
  ui/fastOptJS                                            \
  seqexec_web_client/fastOptJS::webpack

# Remember how this turned out
EXIT_CODE=$?

# clean up docker image
docker stop $CID
docker rm --volumes --force $CID

# done
exit $EXIT_CODE


