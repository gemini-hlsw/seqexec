#!/bin/bash
cd $TRAVIS_BUILD_DIR/modules/ui/target/scala-2.12/
python $TRAVIS_BUILD_DIR/build/weigh_in.py gem-ui-fastopt.js
python $TRAVIS_BUILD_DIR/build/weigh_in.py gem-ui-opt.js
