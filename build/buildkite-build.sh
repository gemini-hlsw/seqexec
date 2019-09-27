#!/bin/bash
set -xe

cd `dirname $0`/..

###
### BUILD
###

echo "--- :scala: Compiling main codebase"
/usr/local/bin/sbt                  \
  -jvm-opts build/buildkite-jvmopts \
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
  test:compile

###
### RUN TESTS
###

# Set up the schema generate enums and run tests
echo "--- :scala: Running tests"
/usr/local/bin/sbt                                        \
  -jvm-opts build/buildkite-jvmopts                       \
  -Docs3.skipDependencyUpdates                            \
  test

###
### JAVASCRIPT PACKAGING
###

echo "--- :javascript: Linking Javascript"
/usr/local/bin/sbt                      \
  -jvm-opts build/buildkite-jvmopts     \
  seqexec_web_client/fullOptJS::webpack

###
### WEIGH
###
if [ "$BUILDKITE" = "true" ] && [ -n "$GITHUB_TOKEN" ] && [ -n "$BUILDKITE_PULL_REQUEST" ]; then
  echo "--- :github: Calculate assets' sizes"
  $BUILDKITE_BUILD_CHECKOUT_PATH/build/weigh.sh
fi
