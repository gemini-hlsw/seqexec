# gemctl

This is a deployment "script" for Gem based on [Docker](). It lets us deploy Gem (telnetd for now since there's not a web front-end yet) locally or on a remote machine, upgrade an existing deployment, and do some minimal monitoring.

## Setting Up

#### Install Docker

First [install Docker for Mac](https://docs.docker.com/docker-for-mac/install/). This will give you the services and commandline doodads you need.

#### Setting Up to Deploy Locally

For day to day development it's convenient to deploy locally, which means the Docker containers are running in your local machine's Docker engine, using your local image repository. All you need to do to make this work is have Docker running.

#### Setting Up to Deploy to a Local VM

We're going to use a local VM as a "practice" server, and we will create containers inside this VM. That way it's easy to clean up and make sure we're doing everything in an isolated environment that acts like a real server.

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

And then copy your public key over. I will assume you already have one. The password for user `docker` on the new VM is `tcuser`.

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

From the sbt shell.
