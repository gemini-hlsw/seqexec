
### Running Setup

The server has two containers, one running gem and one running Postgres.

TODO: read replica on another machine
TODO: standby system on another machine with log streaming from master
TODO: cron for backup

### Upgrade Procedure

To upgrade we can't just shut down the old instance; we need to move data by streaming between Postgres instances so the transfer of power has to be orderly.

This will need to be bootstrapped manually but should be automatic after the first deploy.

`docker ps` will show two images with names

    gem-<n> // the gem server
     db-<n> // the database server, which may have

where `<n>` is the deployment number, which increments with each successful deployment. If there is any other output then it's an error and we stop and a human has to figure out what happened.

Both will be running on the private bridge network `gem-net` but only `gem-<n>` will have public ports.

- Stop `gem-<n>`. This cuts off network traffic and isolates the database. There is no persistent state in the gem server so there's no need to be gentle about it.

- Start a new database server `pg-<n+1>` from the standard image. We will provide `-v /host/path/<n+1>:/container/default/storage/path` which will cause postgres to store its data on the host system in a namespaced directory. This way it's "real" data on a real filesystem.

- `docker run` another copy of the pg image with a command to copy from `pg-<n>` to `pg-<n+1>`. On completion `pg-<n>` and `pg-<n+1>` will be identical. We can use the same mechanism on a cron job to do backup dumps onto long-term storage.

- Start a new gem server `gem-<n+1>` from the newly built image, with only local telnet published, passing `-e GEM_DB_HOST=db-<n+1>`.

- Hit the new server with telnet to get a health report. If all is ok restart `gem-<n+1>` with all services exposed, otherwise send telnet results (if any) and logs from `gem-<n+1>` to admins somehow (by opening a GH issue?) and roll back.

- If all went well remove the images and database storage for `<n-M>` for some number of generations `M` that we want to keep as a rollback target.

### Rollback Procedure

- Stop `gem-<n+1>` and `pg-<n+1>` and remove the containers and database storage.
- Start `gem-<n>` and `pg-<n>`.

### Restart with Backup Database

...

### Production Differences

For production the database server will have on-disk storage that will need to be namespaced with the deployment id. This will need to be sent as a parameter when starting up. On failed deployment it will need to be cleaned up.

We could also experiment with read-only access or log streaming to avoid downtime during upgrades, but this may not be necessary. Depends on the frequency of upgrades I gusss.
