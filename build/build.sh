#!/bin/bash
set -xe

cd `dirname $0`/..

sbt -jvm-opts travis-jvmopts -Docs3.skipDependencyUpdates headerCheck test:headerCheck scalastyle sql/flywayMigrate compile test ui/fastOptJS seqexec_web_client/fastOptJS::webpack
