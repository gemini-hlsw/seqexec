#!/bin/bash
cd $TRAVIS_BUILD_DIR/modules/edu.gemini.seqexec.web/edu.gemini.seqexec.web.client/target/scala-2.12/scalajs-bundler/main/

# upload merged
if [ -z "${AWS_ACCESS_KEY_ID}" ]; then
    aws s3 cp edu_gemini_seqexec_web_client-fastopt.js s3://gemartifacts/seqexec/$TRAVIS_COMMIT/ --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers
    aws s3 cp edu_gemini_seqexec_web_client-fastopt-library.js s3://gemartifacts/seqexec/$TRAVIS_COMMIT/ --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers
fi

# upload tagged versions
if [ -z "${TRAVIS_TAG}" ]; then
    aws s3 cp edu_gemini_seqexec_web_client-fastopt.js s3://gemartifacts/seqexec/$TRAVIS_TAG/ --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers
    aws s3 cp edu_gemini_seqexec_web_client-fastopt-library.js s3://gemartifacts/seqexec/$TRAVIS_TAG/ --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers
fi
