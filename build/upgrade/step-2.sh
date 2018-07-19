#!/bin/bash

# This dynamically generates a pipeline containing a click to unblock step with
# custom fields.

set -e

DEPLOY_SERVER=`buildkite-agent meta-data get deploy-server`
DEPLOY_VERSION=`buildkite-agent meta-data get deploy-version`

echo "--- :docker: Deploying $DEPLOY_VERSION to $DEPLOY_SERVER"
/usr/local/bin/sbt                   \
  -jvm-opts build/buildkite-jvmopts  \
  -Docs3.skipDependencyUpdates       \
  ctl/compile
  "ctl/runMain gem.ctl.main --verbose --host $DEPLOY_SERVER deploy-production $DEPLOY_VERSION"


