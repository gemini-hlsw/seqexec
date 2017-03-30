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

The more general situation is that you want to poke a remote machine, and it can sometimes be helpful to set up a "remote" machine running on a local VM, just to guarantee an additional level of isolation. **You can skip this step if you want to.**

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

#### Quick Start

Deployments can be upgrades (the default) or standalone. Since we have no deployment to upgrade yet we'll do the latter. At the sbt prompt do:

```
> gemctl deploy -s
```

Lots of things will happen and some things will be downloaded from the internet, and eventually it will end in ... failure. Because we haven't created an application image to deplot. To do this do:

```
> telnetd/docker:publishLocal
```

And then try `gemctl deploy -s` again. It should work!

```
> gemctl deploy -s
[info] Running gem.ctl.main deploy -s

[info]  Target host is localhost
[info]  Deploy number for this host will be 1.
[info]  Verifying deploy commit.
[info]    Using HEAD.
[info]    Commit is cb5d0a4095775b3f71665735c4a110a0b9b49805
[warn]    There are uncommitted changes. This is a UNCOMMITTED deployment.
[info]  Verifying Postgres deploy image.
[info]    Looking for postgres:9.6.0
[info]    Image is bff88803c2df
[info]  Verifying Gem deploy image.
[info]    Looking for geminihlsw/gem-telnetd:cb5d0a4095775b3f71665735c4a110a0b9b49805-UNCOMMITTED
[info]    Image is b20fc231f585
[info]  Verifying gem-net network.
[info]    Using existing network 3c89948d2a81.
[info]  Performing STANDALONE deployment
[info]    Ensuring that there is no running deployment.
[info]      There are no running deployments.
[info]    Deploying database.
[info]      Creating Postgres container from image bff88803c2df
[info]        Container is db-1 9a98dbbf272fa125cab9294c904152105774dc3f3497e17b20ec473d973ee057.
[info]      Waiting for Postgres to start up.
[info]      Waiting for Postgres to start up.
[info]      Created database.
[info]      Waiting for health check.
[info]      Waiting for health check.
[info]      Waiting for health check.
[info]      Container is healthy.
[info]    Deploying Gem.
[info]      Creating Gem container from image b20fc231f585
[info]        Container is gem-1 7a8982ff3611e5319083b51d5f2ab157302d51624967064a713e2dc9a38d0a7f.
[info]      Service is available at localhost:1234.
```

Log in as root with no password, then change the password.

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

Now let's upgrade to a new deployment.
