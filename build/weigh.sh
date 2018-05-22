#!/bin/bash
cd $TRAVIS_BUILD_DIR/modules/edu.gemini.seqexec.web/edu.gemini.seqexec.web.client/target/scala-2.12/scalajs-bundler/main/
python $TRAVIS_BUILD_DIR/build/weigh_in.py seqexec-library.js
python $TRAVIS_BUILD_DIR/build/weigh_in.py edu_gemini_seqexec_web_client-fastopt-library.js
# python $TRAVIS_BUILD_DIR/build/weigh_in.py edu_gemini_seqexec_web_client-opt.js
# python $TRAVIS_BUILD_DIR/build/weigh_in.py edu_gemini_seqexec_web_client-opt-library.js
