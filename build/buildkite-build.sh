#!/bin/bash
set -xe

cd `dirname $0`/..

###
### BUILD
###

echo "--- :scala: Compiling main codebase"
/usr/local/bin/sbt                  \
  -jvm-opts build/buildkite-jvmopts \
  -Docs3.skipDependencyUpdates      \
  headerCheck                       \
  test:headerCheck                  \
  scalastyle                        \
  compile

###
### COMPILE TESTS
###

# Compile tests
echo "--- :scala: Compiling tests"
/usr/local/bin/sbt                                        \
  -jvm-opts build/buildkite-jvmopts                       \
  -Docs3.skipDependencyUpdates                            \
  -Docs3.databaseUrl=jdbc:postgresql://$HOST_AND_PORT/gem \
  test:compile

###
### RUN TESTS
###

# Start a new Postgres container for this build
# TODO: read postgres version from the build
echo "--- :postgres: Starting Postgres test instance"
CID=`docker run --detach --publish 5432 postgres:9.6.0`

# Add an exit handler to ensure we always clean up.
function cleanup {
  # Unfold the previous (failing) group if we're exiting abnormally
  if [[ $? -ne 0 ]]; then
    echo "^^^ +++"
  fi
  echo "--- :postgres: Cleaning up Postgres test instance"
  docker stop $CID
  docker rm --volumes --force $CID
}
trap cleanup EXIT

# Get our host and port ... like 0.0.0.0:32751
HOST_AND_PORT=`docker port $CID 5432/tcp`

# The postgres user already exists, so we can go ahead and create the database
echo "--- :postgres: Creating database"
until docker exec $CID psql -U postgres -c 'create database gem'
do
  sleep 1
done

# Set up the schema generate enums and run tests
echo "--- :scala: Running tests"
/usr/local/bin/sbt                                        \
  -jvm-opts build/buildkite-jvmopts                       \
  -Docs3.skipDependencyUpdates                            \
  -Docs3.databaseUrl=jdbc:postgresql://$HOST_AND_PORT/gem \
  sql/flywayMigrate                                       \
  genEnums                                                \
  test

# Check git status. if genEnums generated something mismatching, this should fail
if [ "$BUILDKITE" = "true" ]; then
  echo "--- :git: check code generation is current"
  if [ -n "$(git status --porcelain)" ]; then
    # Uncommitted changes in tracked files
    echo "--- :shrug: Enum generation produced unmatching files"
    exit 1
  fi
fi

###
### JAVASCRIPT PACKAGING
###

echo "--- :javascript: Linking Javascript"
/usr/local/bin/sbt                      \
  -jvm-opts build/buildkite-jvmopts     \
  -Docs3.skipDependencyUpdates          \
  seqexec_web_client/fastOptJS          \
  ui/fastOptJS

echo "--- :webpack: Webpack"
/usr/local/bin/sbt                      \
  -jvm-opts build/buildkite-jvmopts     \
  -Docs3.skipDependencyUpdates          \
  seqexec_web_client/fastOptJS::webpack \
  seqexec_web_client/fullOptJS::webpack

###
### WEIGH
###
if [ "$BUILDKITE" = "true" ] && [ -n "$GITHUB_TOKEN" ] && [ -n "$BUILDKITE_PULL_REQUEST" ]; then
  echo "--- :github: Calculate assets' sizes"
  $BUILDKITE_BUILD_CHECKOUT_PATH/build/weigh.sh
fi

###
### DOCKER IMAGE
###

# If this is a merge into `develop` then this is a shippable version and we will build a docker
# image for it. We can later deploy it to test or production.
if [ "$BUILDKITE_PULL_REQUEST" = "false" ] && [ "$BUILDKITE_BRANCH" = "develop" ]; then

  echo "--- :docker: Creating a Docker image"
  /usr/local/bin/sbt                      \
    -jvm-opts build/buildkite-jvmopts     \
    -Docs3.skipDependencyUpdates          \
    main/docker:publish                   \
    main/docker:clean

  echo "--- :docker: Deploying to the test environment "
  /usr/local/bin/sbt                      \
    -jvm-opts build/buildkite-jvmopts     \
    -Docs3.skipDependencyUpdates          \
    ctl/deployTest

fi
