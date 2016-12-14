
## Running Gem with Docker

This document explains how to deploy Gem inside docker on localhost. A next step is getting this tooling to deploy and manage remote machines, but it doesn't do that yet.

### About Docker

Docker ("docker engine") is a system service that manages **containers**. A container is a program that runs with its own Linux environment, but all containers on a machine share the same kernel. So it's kind of like a VM but it's better to think of as a unit of isolation for applications.

Containers are based on **images** which are immutable filesystem layers that define an application's environment. The application itself is added on top of an existing base image like `ubuntu` or `centos` for instance. These are publicly available.

The running state of our system is two docker containers, one running Gem (just telnetd for now) and one running Postgres. The Postgres container uses the `postgres:9.6.0` image which is maintained by Postgres. The Gem container uses the `java:latest` base image with our application added on via sbt-native-packager, yielding (for now) an image called `telnetd:0.1-SNAPSHOT`.

### Getting Started

Ok what we want to do is `./gemctl deploy` to make our first deployment. So try that.

    gem$ ./gemctl deploy
    [gemctl] ERROR: Docker isn't installed or isn't running. Download at https://www.docker.com/
    gem$

Ok so go install Docker, start it, and try again.

    gem$ ./gemctl deploy
    [gemctl] Creating gem-net bridge network.
    [gemctl] ERROR: Could not find telnetd:0.1-SNAPSHOT image. You may need to do 'sbt docker:publishLocal'.
    gem$

We got a little farther this time. Looks like we need to publish our image. This will build the app and add it to our local Docker image repository. So do what it says and try again.

    gem$ ./gemctl deploy
    gem (docker *)$ ./gemctl deploy
    [gemctl] Found gem-net bridge network.
    [gemctl] Found telnetd:0.1-SNAPSHOT image.
    [gemctl] This is the first deployment on localhost.
    [gemctl] Deploying Postgres container db-1.
    [gemctl] Creating database.
    [gemctl] Creating Gem container gem-1.
    [gemctl] Telnet is exposed on 1234.
    [gemctl] TODO: Import existing data. We will need a mount point and machinery for this.
    [gemctl] Verifying deployment 1.
    [gemctl] Deployment 1 database looks ok.
    [gemctl] Deployment 1 Gem server looks ok.
    gem$

And test it:

    gem$ telnet localhost 1234
    Trying ::1...
    Connected to localhost.
    Escape character is '^]'.
    Welcome to Gem
    Username: root
    Password:
    Welcome, local time is Tue Dec 13 21:58:36 UTC 2016.
    root@gem> exit
    Goodbye.
    Connection closed by foreign host.
    gem$

We can look at the running containers.

    gem$ docker ps
    CONTAINER ID        IMAGE                  COMMAND                  CREATED              STATUS              PORTS                    NAMES
    e4b079b1c2a7        telnetd:0.1-SNAPSHOT   "bin/telnetd"            About a minute ago   Up About a minute   0.0.0.0:1234->6666/tcp   gem-1
    2231cdc6ef9e        postgres:9.6.0         "/docker-entrypoint.s"   About a minute ago   Up About a minute   5432/tcp                 db-1

We can look at the logs (^C to quit).

    gem$ docker logs -f gem-1
    Connecting with URL jdbc:postgresql://db-1/gem, user postgres, pass «hidden»
    Dec 13, 2016 9:57:41 PM org.flywaydb.core.internal.util.VersionPrinter printVersion
    INFO: Flyway 4.0.3 by Boxfuse
    Dec 13, 2016 9:57:41 PM org.flywaydb.core.internal.dbsupport.DbSupportFactory createDbSupport
    ...

We can run a command "in" the running container.

    gem$ docker exec gem-1 ls lib
    com.chuusai.shapeless_2.11-2.3.1.jar
    com.google.code.findbugs.jsr305-2.0.3.jar
    com.google.guava.guava-17.0.jar
    com.googlecode.kiama.kiama_2.11-1.7.0.jar
    ...

Or even run a shell inside the container.

    gem$ docker exec -i -t gem-1 sh
    $ ps -aef
    UID        PID  PPID  C STIME TTY          TIME CMD
    daemon       1     0  1 21:57 ?        00:00:06 /usr/lib/jvm/java-8-openjdk-amd64/bin/java -cp /opt/docker
    daemon      94     0  0 22:05 ?        00:00:00 sh
    daemon      99     0  0 22:05 ?        00:00:00 sh
    daemon     106    99  0 22:05 ?        00:00:00 ps -aef
    $ exit
    gem$

Or run `psql` in the database container.

    gem$ docker exec -i -t db-1 psql -U postgres -d gem
    psql (9.6.0)
    Type "help" for help.

    gem=# \d
                        List of relations
     Schema |           Name           |   Type   |  Owner   
    --------+--------------------------+----------+----------
     public | dataset                  | table    | postgres
     public | e_f2_disperser           | table    | postgres
     public | e_f2_filter              | table    | postgres
     public | e_f2_fpunit              | table    | postgres
     public | e_f2_lyot_wheel          | table    | postgres
     ...

So these containers act like little servers but they're really just very isolated application environments.

### The Gemctl Tool

The `gemctl` tool is just a wrapper for docker commands, but it's useful. So far it supports the following notions:

- Doing an initial deployment.
- Upgrading an existing deployment to a new version, copying data over.
- Stopping and removing all containers, which can be helpful in testing.

Soon it will support:

- Rolling back to the previous deployment.
- Removing obsolete deployments.

Let's look at supported commands in detail.

#### Initial deployment

The first time you do `gemctl deploy` there are no containers on your system (or at least none that are associated with Gem). So we do the following:

- Create the `gem-net` private network if it doesn't exist.
- Run the `postgres:9.6.0` image to create a new container.
  - Assign it the name `db-1`.
  - Give it metadata labels `gemini.db` and `gemini.deploy=1`.
  - Connect it on `gem-net`.
  - Use `docker exec` to run `psql` on `db-1`, creating the `gem` database.
- Run the `telnetd:0.1-SNAPSHOT` image to create a second container.
  - Assign it the name `gem-1`.
  - Give it metadata labels `gemini.gem` and `gemini.deploy=1`.
  - Connect it on `gem-net`.
  - Expose its port 6666 as localhost:1234.
  - Pass `GEM_DB_URL=jdbc:postgresql://db-1/gem` in the environment. This is how it knows which database to talk to.
- Double-check to see that the database and telnet server are both accepting connections.

Note that there's not yet a way to populate the database so the only thing you can do right now that touches the database is change the root user password.

#### Upgrade deployment

We can upgrade the current deployment by deploying again. Try it.

    gem$ ./gemctl deploy
    [gemctl] Found gem-net bridge network.
    [gemctl] Found telnetd:0.1-SNAPSHOT image.
    [gemctl] Previous deployment was 1. This will be deployment 2.
    [gemctl] Verifying deployment 1.
    [gemctl] Deployment 1 database looks ok.
    [gemctl] Deployment 1 Gem server looks ok.
    [gemctl] Deploying Postgres container db-2.
    [gemctl] Creating database.
    [gemctl] Copying data from previous deployment.
    [gemctl] Stopping container db-1.
    [gemctl] Stopping container gem-1.
    [gemctl] Creating Gem container gem-2.
    [gemctl] Telnet is exposed on 1234.
    [gemctl] Verifying deployment 2.
    [gemctl] Deployment 2 database looks ok.
    [gemctl] Deployment 2 Gem server looks ok.
    gem$

TODO: more words

### Cleanup

This blows away all your docker containers (but *not* images) so you can start over.

    gem$ ./gemctl clean
    [gemctl] TODO: prompt for confirmation.
    [gemctl] Stopping and removing all containers.
    gem$
