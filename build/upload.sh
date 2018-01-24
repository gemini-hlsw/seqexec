#!/bin/bash
# upload merged
if [ -z "${AWS_ACCESS_KEY_ID}" ]; then
    cd $TRAVIS_BUILD_DIR/modules/ui/target/scala-2.12/
    aws s3 cp gem-ui-fastopt.js s3://gemartifacts/gem/$TRAVIS_COMMIT/ --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers
    aws s3 cp gem-ui-opt.js s3://gemartifacts/gem/$TRAVIS_COMMIT/ --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers
fi

# upload tagged versions
if [ -z "${TRAVIS_TAG}" ]; then
    cd $TRAVIS_BUILD_DIR/modules/ui/target/scala-2.12/
    aws s3 cp gem-ui-fastopt.js s3://gemartifacts/gem/$TRAVIS_TAG/ --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers
    aws s3 cp gem-ui-opt.js s3://gemartifacts/gem/$TRAVIS_TAG/ --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers
fi
