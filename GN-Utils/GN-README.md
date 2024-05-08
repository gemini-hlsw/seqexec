# Gemini North Seqexec

This document is intended to detail the procedure to build the seqexec for Gemini North. At Gemini South a dedicated build server running MacOS is used. At GN we do not have a dedicated build server and therefore will build using either one of two methods.
1. Build locally on Mac development machine
2. Build within an Ubuntu docker container running on a Rocky8 host development machine

## Method 1. Build locally on Mac

### Install dependencies:

Install `sdkman` to manage your sbt, scala, and java versions. I tried to manually manage my versions using brew and ran into many issues.

* `$ curl -s "https://get.sdkman.io" | bash`

Now you can install sbt (scala build tool)
* `sdk install sbt`

The version of java you have is probably too new. I needed java17 and node16.

Install the java version you need
* `sdk list java`
* `sdk install java`

You can use sdkman to easily switch between java versions.

My version of node.js was also too new so I had to install an older version. Had to use brew, unfortunately sdkman does not manage node.js
* `brew install node@16`
* `brew unlink node`
* `brew link --force --overwrite node@16`
* `node --version`

Now you can start sbt
* `sbt`

In the case of this project you will have memory issues with the default settings, instead start sbt with the following arguments
* `sbt -J-Xmx2G -J-XX:MaxMetaspaceSize=512M`

Now you can build the project
* `sbt clean`
* `app_seqexec_server_gn_test/compile`
* `app_seqexec_server_gn_test/stage`
* `app_seqexec_server_gn_test/universal:packageZipTarball`

## Method 2. Build in Docker Container

The docker container doesn't work on an M1 mac so I had to run it on a Rocky8 Linux machine.

In the repo are two docker files `Dockerfile.base` and `Dockerfile.dev`

Build the base version first and then the dev version. Issueing the build command will generate the tarball and save it in the container. Future work will be to mount a volume to the container and have the built tarball saved to a directory on the dev machine.
```
docker build -t registry.gitlab.com/nsf-noirlab/gemini/rtsw/user-tools/seqexec-tester/seqexec-base -f Dockerfile.base .

docker build -t registry.gitlab.com/nsf-noirlab/gemini/rtsw/user-tools/seqexec-tester/seqexec-dev -f Dockerfile.dev .
```