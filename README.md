# Seqexec Web

This set of modules contains a web server and the client for the seqexec

## Web backend

The backend is written with [http4s](http://http4s.org), it exposes a REST API and can provide static files for the UI. It is intended to run in the same process as the seqexec-server

## sbt-revolver

This project uses an extra plugin

* [sbt-revolver](https://github.com/spray/sbt-revolver): This plugin allows to restart the web server and trigger a recompilation when the source code changes

## How to compile and start the server (Good for backend development)

Go to the JVM project

```
    project seqexec_web_server
    ~reStart
```

Now every time a file is changed in the server code, the server files will be compiled, then the server will restart

By default the REST backend will run on port 7070

It can be stopped by executing

```
   reStop
```

from within the project.

# Seqexec Web Client

This module contains a web-based seqexec client. It contains a SPA (Single-page application) which communicates to the backend using Ajax-style calls and websockets

# How to run/develop the client

For the common case we want to develop the client but we also need to run the backend.

an sbt task

```
startSeqexecAll
```

Will do the following:

* Launch the backend on the background
* Pack the client going through scala.js and webpack
* Launch webpack-dev-server with a proxy to the backend

Now you can open the client at

http://localhost:9090

if you want to update the client and get automatic reload do in sbt:

```
    project seqexec_web_client
    ~fastOptJS
```

and to stop all the processes you can do

```
stopSeqexecAll
```
