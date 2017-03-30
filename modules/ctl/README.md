# gemctl

This is a deployment "script" for Gem based on [Docker](). It lets us deploy Gem (telnetd for now since there's not a web front-end yet) locally or on a remote machine, upgrade an existing deployment, roll back to the previous deployment, and do some minimal monitoring.

You **do not need to use this** for day to day development, but it can be useful for testing upgrades. You could for instance have the current production version deployed locally (or on a VM), and test upgrading to your local branch, rolling back after each test.

The big idea is that we use Docker to run the application and database in a little isolated environment that lets us "install" multiple instances concurrently, and upgrade one to the next in a safe way by streaming data from the old instance to the new one. This lets us roll back quickly if something goes wrong.

## Setting Up

#### Install Docker

First [install Docker for Mac](https://docs.docker.com/docker-for-mac/install/). This will give you the services and commandline doodads you need. I suggest working through the getting started tutorial at some point just to get a sense for Docker, but the tl;dr is that it provides isolated environments for applications. It's not a VM technology but it provides a high level of isolation that behaves similarly in some ways.

#### Setting Up to Deploy Locally

For day to day usage it's convenient to deploy locally, which means the Docker containers are running in your local machine's Docker engine, using your local image repository. This means when you package up the app to deploy you can use `docker:publishLocal` and nothing has to go over the network. All you need to do to make this work is have Docker running.

#### Setting Up to Deploy to a Local VM

The more general situation is that you want to poke a remote machine, and it can sometimes be helpful to set up a "remote" machine running on a local VM, just to guarantee an additional level of isolation. **You can skip this step if you want to. Come back and do it later if you want to, we won't be relying on this.**

Ok, so `docker-machine` is a utility makes it a little easier to provision VMs that are going to be running Docker. All it's doing is remote-controlling VirtualBox and it's all installed as part of Docker so we'll use it. We could just as easily create a VM manually with the `boot2docker` ISO and it would work fine.

Anyway. Let's create a new virtual machine called `default`. When you issue a command to `docker-machine` it assumes you're talking about a machine called `default` unless you say otherwise, so using this name will save us some typing.

```
$ docker-machine create --driver virtualbox default
Running pre-create checks...
...
Checking connection to Docker...
Docker is up and running!
To see how to connect your Docker Client to the Docker Engine running on this virtual machine, run: docker-machine env default
$ _
```

If you search for VirtualBox on your machine you will find that it's there and you'll see this newly-created machine in the UI.

The next thing we need to do is set up SSH so we can log into this machine without a password. First get the IP address for `default`.

```
$ docker-machine ip
192.168.99.100
$ _
```

And then copy your public key over. I will assume you already have one. The password for user `docker` on the new VM is `tcuser`. Note that you will probably need to re-do this step if you stop and re-start the machine.

```
$ cat ~/.ssh/id_rsa.pub | ssh docker@192.168.99.100 'cat >> .ssh/authorized_keys'
The authenticity of host '192.168.99.100 (192.168.99.100)' can't be established.
ECDSA key fingerprint is SHA256:xPMlSBTa1YWhGXGGYRiv13rkKcHrcCZGfe2Owuuln8U.
Are you sure you want to continue connecting (yes/no)? yes
Warning: Permanently added '192.168.99.100' (ECDSA) to the list of known hosts.
docker@192.168.99.100's password:
$ _
```

Verify that you can `ssh` into the VM.

```
$ ssh docker@192.168.99.100
                        ##         .
                  ## ## ##        ==
               ## ## ## ## ##    ===
           /"""""""""""""""""\___/ ===
      ~~~ {~~ ~~~~ ~~~ ~~~~ ~~~ ~ /  ===- ~~~
           \______ o           __/
             \    \         __/
              \____\_______/
 _                 _   ____     _            _
| |__   ___   ___ | |_|___ \ __| | ___   ___| | _____ _ __
| '_ \ / _ \ / _ \| __| __) / _` |/ _ \ / __| |/ / _ \ '__|
| |_) | (_) | (_) | |_ / __/ (_| | (_) | (__|   <  __/ |
|_.__/ \___/ \___/ \__|_____\__,_|\___/ \___|_|\_\___|_|
Boot2Docker version 17.03.0-ce, build HEAD : f11a204 - Thu Mar  2 00:14:47 UTC 2017
Docker version 17.03.0-ce, build 3a232c8
docker@default:~$ exit
Connection to 192.168.99.100 closed.
$ _
```

You can do `docker-machine stop` to shut down the VM and `docker-machiner start` to start it back up again. Anyway we're now all set for a "remote" deployment.

## Deploying and Upgrading Gem

Ok so `gemctl` is an alias in the build that lets you run deployment stuff from the sbt prompt. If you just type `gemctl` at the sbt prompt you'll get a help message. `gemctl -h` will list available commands, and `gemctl <command> -h` will show details on any specific command. If the help messages aren't helpful, please improve them.

#### Packaging the Application

In order to deply the app we must first create a **docker image** of our application. We do this via sbt-native-packager as follows:

```
> telnetd/docker:publishLocal
```

Image names have the form `<repo>/<artifact>:<version>`. Ours will be `geminihlsw/gem-telnetd:<version>` where the version is the git commit hash, or the hash followed by `-UNCOMMITTED` if there are uncommitted changes. This give us very fine-grained versioning and a reliable partial ordering to know that an upgrade is legal. Note that if the filesystem version isn't the same as the version computed by sbt you'll need to reload the project. The prompt will include a message to this effect.

The image will be stored in docker's internal storage area, which shares "slices" with other images built from the same base to save space.

For remote deployments we must publish and push the image to a repository where it can be seen by other machines. For now we're using docker-hub but it probably makes sense to set up an internal repository. To do this you need to create a docker-hub account and be added to the `geminihlsw` organization (talk to Rob), and then you can do `telnetd/docker:publish` to push the image up to the cloud. Note that you need to do this in order to perform remote deployments, even if they're to a VM on your own machine.

#### Initial Deployment

Deployments can be upgrades (the default) or standalone. Since we have no deployment to upgrade yet we'll do a standalone deployment. At the sbt prompt do:

```
> gemctl deploy -s
```

By default the deployment will use the current git HEAD as the version, and will deploy locally. Lots of things will happen and some things will be downloaded from the internet, and eventually you should end up with a running application. The log is pretty informative and shows the various things that are being done. If you run with `-v` you will see all the commands that are being issued.


```
> gemctl deploy
[info] Updating {file:/Users/rnorris/Scala/gem/}ctl...
[info] Resolving jline#jline;2.12.1 ...
[info] Done updating.
[info] Running gem.ctl.main deploy

[info]  Target host is localhost
[info]  Deploy number for this host will be 3.
[info]  Verifying deploy commit.
[info]    Using HEAD.
...
[info]    Deploying Gem.
[info]      Creating Gem container from image 31f970aef7c7
[info]        Container is gem-3 8a6a3e54ef47ba3e79ce1b473939d74cd83c1c786d137a2be30d54cb226d872f.
[info]      Service is available at localhost:1234.

[success] Total time: 18 s, completed Mar 30, 2017 4:09:32 PM
>
```

Log in as root with no password, then change the password just to commit a change to the database.

```
gem (docker-2 *)$ telnet localhost 1234
Trying ::1...
Connected to localhost.
Escape character is '^]'.
Welcome to Gem
Username: root
Password:
Welcome, local time is Thu Mar 30 21:33:45 UTC 2017.
root@gem> passwd
Old password:
New password: ******
Password changed.
root@gem> exit
Goodbye.
Connection closed by foreign host.
```

#### Look at Logs

You can use `gemctl log` to view the tail of the server log.

```
> gemctl log
[info] Running gem.ctl.main log

[info]  Target host is localhost
[info]  Finding current running Gem container.
[shel]  Connecting with URL jdbc:postgresql://db-4/gem, user postgres, pass «hidden»
[shel]  Mar 30, 2017 11:09:54 PM org.flywaydb.core.internal.util.VersionPrinter printVersion
[shel]  INFO: Flyway 4.0.3 by Boxfuse
[shel]  Mar 30, 2017 11:09:55 PM org.flywaydb.core.internal.dbsupport.DbSupportFactory createDbSupport
...
[shel]  Mar 30, 2017 11:09:57 PM net.wimpi.telnetd.net.PortListener run
[shel]  INFO: Listening to Port 6,666 with a connectivity queue size of 5.

[success] Total time: 1 s, completed Mar 30, 2017 4:43:23 PM
>
```

#### Upgrade Deployments

Now let's upgrade to a new deployment.

```
> gemctl deploy -f`
```

The `-f` allows an upgrade to have the same version as the running version, which normally wouldn't make any sense but can be helpful for development. Note that this is always allowed for `-UNCOMMITTED` versions since it knows you're just working locally.

Note that it copies the old database over before shutting things down, so if you telnet in again you will see that the root password has the value you set last time.

#### Rolling Back

There is minimal support for rolling back to a previous version, in case a critical failure is discovered soon after deployment. You can simply say

```
> gemctl rollback
```

to go back to the previous version. Each deployment has back-pointers, but not forward pointers. If you want to roll forward you will need to do `gemctl stop` and then go to the shell and use `docker ps -a` to find the newer containers you wish to start back up (then `docker start <container>` for each). We can automate this (and should).
