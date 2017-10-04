
# Gem Build/Deployment Setup

## Overview

## Build Environment

Developers can build and run with sbt and a local Postgres install, which is the usual setup. It is also possible to do any kind of deployment on a local docker instance. This can be useful for testing changes to the deployment setup itself, or to bring up an older version of Gem locally without disturbing your development environment.

The **continuous integration** environment is Jenkins, running (in a docker container) on `gem-dev`. It will automatically build and push images for every merge to `master`, and automatically deploy to `gem-test`. There are on-demand tasks to deploy archived versions on `gem-arch` and deploy/rollback on `gem-prod`.

## Versioning

Gem has no versioning scheme as such. Everything is based on **git commit hashes**, and upgrade compatibility is verified based on lineage: you can upgrade A to B if and only if commit A is an ancestor of commit B. A version is identified by the [redundant] epoch year and short commit hash, like `2017.735-9a41912` which allows a user to sort them by relative date.

> **Discussion**. An epoch year with three decimal digits has a resolition of about 8 hours; versions falling in the same block will have to be disambiguated via `git show` to determine the full timestamp. This scheme is a compromise that is readable and typeable, and uses characters that are valid in all cases where the version label is used. We can add another digit if we like.

We indicate **local changes** with a trailing `-dirty`, so a version like `2017.735-9a41912-dirty` indicates a build with local uncommitted changes. Such builds can only be deployed locally and cannot be upgraded.

## Build Artifacts

The build artifact is a docker image.

- The name of the image will be `gem` with a version tag: `gem/2017.735-9a41912`
- The image will be constructed by sbt-native-packager, based on a Java base image that can change as Java evolves.
- Labels will indicate:
  - The "version" string.
  - The full commit hash.
  - The name and version of the postgres image we expect. This is specified in the build and can be upgraded simply by changing the version.
  - A list of all commits in this build's history; i.e., a list of all versions whose databases dumps are readable by this version.

The CI server will build images and push them to our internal docker hub automatically.

## Deployments

A deployment consists of a pair of containers on the same host:

- A container based on a Gem image, which exposes its web and telnet ports.
- A container based on the Postgres image specified in the Gem image's metadata. A cron job in this container will stream `pg_dump` backups to a reliable remote filesystem.

Container names are the version as specified above, with a suffix `-G` for the Gem container and `-P` for the Postgres container.

#### Bootstrap Deployments

A **bootstrap deployment** simply sets up the containers and starts them up, with a empty database. There must be no current running deployment.

> **Discussion**. We verify there is no current running deployment simply by checking that all required ports are unbound.


#### Archive Deployments

An **archive deployment** is like a bootstrap deployment, but before starting up the Gem container we populate the database from a compatible backup (i.e., a backup from any database whose version appears in this deployment's history).

If the deployment and backup have the same commit then it's equivalent to going back in time and looking at the system at the time the backup was performed.

The **test deployment strategy** uses archive deployments with the latest production backup.

#### Upgrade Deployments

An **upgrade deployment** *requires* a current running deployment, which we gradually shut down, streaming data to the new instance as it comes up. A final backup can be done concurrently via `tee` or shortly thereafter if it slows things down too much. The goal is to minimizes downtime. This is the **production deployment strategy**.

Upgrade deployments are eligible for **rollback**. When we upgrade we add a label to the Gem container indicating the prior container, which we can start back up if the new deployment is found to be defective. If the upgrade included no schema changes we can in principle perform a **soft rollback** and migrate data back to the prior instance. Otherwise we must perform a **hard rollback** which loses interim changes.

> **Discussion**. We can tag everything with a hash of the Flyway upgrade scripts via `tar -cf- modules/sql/src/main/resources/db/migration | md5` which will let us determine mechanically whether versions are schema-compatible.

## Other Maintenance

We will want to remove old deployments once they have been down for a while, since each has a copy of the entire database.

## Upgrading this Process
