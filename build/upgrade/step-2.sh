#!/bin/bash
set -xe

# These are set by step-1 which blocks the pipeline and prompts for these values.
DEPLOY_SERVER=`buildkite-agent meta-data get deploy-server`
DEPLOY_VERSION=`buildkite-agent meta-data get deploy-version`

# There is no way to pass an empty string as a property, so we pass a flag instead.
if [ `buildkite-agent meta-data get deploy-verbose` = "true" ]; then
  DEPLOY_VERBOSE="--verbose"
fi

# First build gemctl
echo "--- :scala: Compiling gemctl"
/usr/local/bin/sbt                   \
  -jvm-opts build/buildkite-jvmopts  \
  -Docs3.skipDependencyUpdates       \
  ctl/compile

# Now do the deploy on another invocation of sbt. Slightly slower but the logs are much cleaner.
echo "--- :docker: Deploying $DEPLOY_VERSION to $DEPLOY_SERVER"
/usr/local/bin/sbt                   \
  -jvm-opts build/buildkite-jvmopts  \
  -Docs3.skipDependencyUpdates       \
  "ctl/runMain gem.ctl.main $DEPLOY_VERBOSE --host $DEPLOY_SERVER deploy-production $DEPLOY_VERSION"


