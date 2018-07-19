#!/bin/bash

# This dynamically generates a pipeline containing a click to unblock step with
# custom fields.

set -e

DEPLOY_SERVER=`buildkite-agent meta-data get deploy-server`
DEPLOY_VERSION=`buildkite-agent meta-data get deploy-version`

echo "--- :docker: Deploying $DEPLOY_VERSION to $DEPLOY_SERVER"
echo "done"


