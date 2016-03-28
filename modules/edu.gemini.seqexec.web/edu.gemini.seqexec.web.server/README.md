# Seqexec Web Server

This bundle contains a web server for the seqexec, it exposes a REST API and can provide static files for the UI. It is intended to run in the same process as the seqexec-server

## Web backend

In this version we are experimenting with two backends, http4s and play embedded. 

Each backend exposes the same REST API so the client can use either one. The API includes:

* One end point to GET comments
* One end point to POST new comment
* WebSockets endpoint that sends a Ping-like message
* Support to deliver static files and scala.js generated files

### Play backend

[Play](https://www.playframework.com) is a well established scala web framework, however it took the view that the world was centered on play making it next to impossible to integrate in into the OCS. Starting on version 2.4 it is possible to embed a Play server in other application. see [Scala Embedding](https://www.playframework.com/documentation/2.4.x/ScalaEmbeddingPlay)

This makes it a viable option as shown in this project. The play backend simply sets up a NettyServer and the routes for the REST API are created programatically. Since this is a new option, some parts may not be fully documented (e.g. how to setup the initial configuration)

Given the structure of the web application, it doesn't need to use many of the "advanced" features of play, like template generation, css compilation, etc

Give that Play is quite popular it contains good documentation and support for just about anything a web application may require. However, this comes with the cost of bringing many libraries. Hopefully, we can have smaller core since this application doesn't use many of these features.

### http4s backend

[http4s](http://http4s.org/) It is a newer library/framework that is more oriented to lower-level plumbing of web services without support for frontend-oriented features.

It provides enough support for this demo application and it has tighter integration with the scalaz-world. The WebSocket support seems very good.

On the other hand, it feels like a work in progress, with essentially no documentation and doesn't have support for some important features like authentication, form uploads, etc

### Other options

I considered the following other frameworks but discarded them, mostly for their lack of clean WebSocket support:

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
    project bundle_edu_gemini_seqexec_web_server
    ~re-start
```

Now every time a file is changed in either the server or the client, scala.js will compile the javascript files, the server files will be compiled by scalac, then the server will restart

The page can be reached at

[http://localhost:9090/](http://localhost:9090)

## TODO

* Link the application to the Seqexec server
* Authentication
* Review the decision of which backend to use
* Package as a regular application
* Support OSGi