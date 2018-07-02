#!/bin/bash
set -x

# Remove any cruft leftover from the last build.
# Not strictly necessary but let's be safe.
# git clean -xdf

# Start a new Postgres container for this build
# TODO: read postgres version from the build
CID=`docker run --detach --publish 5432 postgres:9.6.0`

# Get our port
PORT=`docker port $CID 5432/tcp | grep -o "[0-9]\+" | tail -n 1`

# Astonishingly there is no reliable way to wait for the container to come up
sleep 5

# The postgres user already exists, so we can go ahead and create the database
docker exec $CID psql -U postgres -c 'create database gem'

# Initialize the database
usr/local/bin/sbt                                       \
 -no-colors                                             \
 -Docs3.skipDependencyUpdates                           \
 -Docs3.databaseUrl=jdbc:postgresql://0.0.0.0:$PORT/gem \
 sql/flywayMigrate

# clean up docker image
docker stop $CID
docker rm --volumes --force $CID

