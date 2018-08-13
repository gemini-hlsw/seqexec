#!/bin/bash
cd $BUILDKITE_BUILD_CHECKOUT_PATH/modules/seqexec/web/client/target/scala-2.12/scalajs-bundler/main/
python $BUILDKITE_BUILD_CHECKOUT_PATH/build/weigh_in.py seqexec-library.js
python $BUILDKITE_BUILD_CHECKOUT_PATH/build/weigh_in.py seqexec_web_client-fastopt-library.js
python $BUILDKITE_BUILD_CHECKOUT_PATH/build/weigh_in.py seqexec.js
python $BUILDKITE_BUILD_CHECKOUT_PATH/build/weigh_in.py seqexec.css
cd $BUILDKITE_BUILD_CHECKOUT_PATH/modules/ui/target/scala-2.12/
python $BUILDKITE_BUILD_CHECKOUT_PATH/build/weigh_in.py ui-fastopt.js
