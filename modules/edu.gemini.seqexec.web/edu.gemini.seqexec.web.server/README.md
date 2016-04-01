# Seqexec Web Server

This bundle contains a web server for the seqexec, it exposes a REST API and can provide static files for the UI. It is intended to run in the same process as the seqexec-server

## Web backend

In this version we are prototyping with two backends, http4s and play embedded. 

Each backend exposes the same REST API so the client can use either one

### Play backend

Starting on version 2.4 it is possible to embed a [Play](https://www.playframework.com) server in other application. see [Scala Embedding](https://www.playframework.com/documentation/2.4.x/ScalaEmbeddingPlay)

This makes it a viable option for integration on the backend. The play backend simply sets up a NettyServer and the routes for the REST API are created programatically. Since this is a new option, some parts may not be fully documented (e.g. how to setup the initial configuration)

Given the structure of the web application, it doesn't need to use many of the "advanced" features of play, like template generation, css compilation, etc

Play is quite popular and it contains good documentation and support for just about anything a web application may require. However, this comes with the cost of bringing many libraries. Hopefully, we can have smaller core since this application doesn't use many of these features.

### http4s backend

[http4s](http://http4s.org/) It is a newer library/framework that is more oriented to lower-level plumbing of web services without support for frontend-oriented features.

It provides enough support for this demo application and it has tighter integration with the scalaz-world. The WebSocket support seems very good.

### Other options

I considered the following other frameworks but discarded them, mostly for their lack of clean WebSocket support and cost of integration:

* [Spray](http://spray.io/) This is a mature framework but it doesn't have built-in support for WebSockets.
* [Scalatra](http://www.scalatra.org/) Seems mature as well though it is mostly meant to run inside a Servlet container and its web socket support is through Atmosphere which may complicate things
* [Finch](https://github.com/finagle/finch) A new framework based on finagle, looks promising but it also lacks clear WebSocket Support

## Static files

This bundle contains the static files for the web fronted. This is a mere convenience making it easier to do development since files can be edited and reloaded without a restart

## sbt-revolver

This project uses an extra plugin

* [sbt-revolver](https://github.com/spray/sbt-revolver): This plugin allows to restart the web server and trigger a recompilation when the source code changes. I consider it essential for web development as it lets you rebuild as soon as files are changed

## How to compile and start the server

Go to the JVM project

```
    project edu_gemini_seqexec_web_server
    ~re-start
```

Now every time a file is changed in either the server or the client, scala.js will compile the javascript files, the server files will be compiled by scalac, then the server will restart

The page can be reached at

[http://localhost:9090/](http://localhost:9090)

## Calling commands

It is possible to directly call commands on the seqexec using a web api. The following commands are supported and include a sample curl command to execute them with the json response
 
# Get host

Returns the ODB host

| Type  | url | parameters |
| :------------- | :------------- | :-- |
| GET  | /api/seqexec/commands/host  ||


```
$: curl  http://localhost:9090/api/seqexec/commands/host
{"command":"host",error:false,"response":"Default seq host set to localhost 8443"}
```

# Set host

Sets the ODB host and port. The parameter `host` must be passed with the format `hostname:port`

| Type  | url | parameters |
| :------------- | :------------- | :-- |
| POST  | /api/seqexec/commands/host  |host|

```
$: curl -X POST -d "host=localhost:8443" http://localhost:9090/api/seqexec/commands/host
{"command":"host localhost:8443",error:false,"response":"Default seq host set to localhost 8443"}
```

*error case:*

```
$: curl -X POST -d "host=localhost8443" http://localhost:9090/api/seqexec/commands/host
{"command":"host localhost8443","error":true,"response":"Sorry, expecting host:port not 'localhost8443'."}
```

# Get sequence count

Returns the amount of steps of a sequence

| Type  | url | parameters |
| :------------- | :------------- | :-- |
| GET  | /api/seqexec/commands/\<obsid>/count  ||

```
curl http://localhost:9090/api/seqexec/commands/GS-2016A-Q-0-1/count
{"command":"show","error":false,"response":"GS-2016A-Q-0-1 sequence has 20 steps."}
```

*error case:*

```
curl http://localhost:9090/api/seqexec/commands/GS-2016A-Q-0-2/count
{"command":"show","error":true,"response":"The database doesn't have observation GS-2016A-Q-0-2"}
```
# Get static configuration

Returns a list with the static configuration of the observation

```
$: curl http://localhost:9090/api/seqexec/commands/GS-2016A-Q-0-1/static
{"command":"show","error":false,"response":"GS-2016A-Q-0-1 Static Values","keys":["instrument:customSlitWidth -> OTHER","instrument:decker -> IMAGING","instrument:disperser -> NONE","instrument:exposureTime -> 85.0","instrument:filter -> OPEN","instrument:fpu -> FPU_NONE","instrument:instrument -> Flamingos2","instrument:issPort -> Side-looking","instrument:lyotWheel -> OPEN","instrument:mosPreimaging -> No","instrument:observingWavelength -> 1.6","instrument:posAngle -> 0.0","instrument:readMode -> FAINT_OBJECT_SPEC","instrument:useElectronicOffsetting -> false","instrument:version -> 2009A-1","observe:class -> science","observe:exposureTime -> 85.0","observe:headerVisibility -> PUBLIC","observe:object -> Untitled","observe:observeType -> OBJECT","observe:proprietaryMonths -> 18","observe:sciBand -> 1","observe:status -> ready","ocs:obsConditions:CloudCover -> 100","ocs:obsConditions:ImageQuality -> 100","ocs:obsConditions:SkyBackground -> 100","ocs:obsConditions:WaterVapor -> 100","ocs:observationId -> GS-2016A-Q-0-1","ocs:programId -> GS-2016A-Q-0","telescope:Base:name -> Untitled","telescope:guideWithOIWFS -> park","telescope:guideWithPWFS1 -> park","telescope:guideWithPWFS2 -> park","telescope:version -> 2009B-1"]}
```