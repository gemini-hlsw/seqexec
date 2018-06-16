#!/bin/bash
set -xe

cd `dirname $0`/..

sbt -jvm-opts travis-jvmopts -Docs3.skipDependencyUpdates headerCheck test:headerCheck scalastyle sql/flywayMigrate
sbt -jvm-opts travis-jvmopts -Docs3.skipDependencyUpdates compile test:compile
sbt -jvm-opts travis-jvmopts -Docs3.skipDependencyUpdates test
sbt -jvm-opts travis-jvmopts -Docs3.skipDependencyUpdates ui/fastOptJS
sbt -jvm-opts travis-jvmopts -Docs3.skipDependencyUpdates seqexec_web_client/fastOptJS::webpack
