#!/bin/bash
cd $TRAVIS_BUILD_DIR/modules/seqexec/web/client/target/scala-2.12/scalajs-bundler/main/
python $TRAVIS_BUILD_DIR/build/weigh_in.py seqexec-library.js
python $TRAVIS_BUILD_DIR/build/weigh_in.py seqexec_web_client-fastopt-library.js
cd $TRAVIS_BUILD_DIR/modules/ui/target/scala-2.12/
python $TRAVIS_BUILD_DIR/build/weigh_in.py ui-fastopt.js
