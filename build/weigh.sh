#!/bin/bash
cd $BUILDKITE_BUILD_CHECKOUT_PATH/modules/seqexec/web/client/target/scala-2.12/scalajs-bundler/main/
python $BUILDKITE_BUILD_CHECKOUT_PATH/build/weigh_in.py seqexec.js
python $BUILDKITE_BUILD_CHECKOUT_PATH/build/weigh_in.py seqexec.css
