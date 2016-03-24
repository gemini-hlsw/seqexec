# Seqexec

This a possible setup for a build for a standalone Seqexec application. This work has the following goals 

- Try to adhere to the simplest possible sbt structure.
- No OSGi support
- Use managed dependencies
- Support for a web application using scala.js
- Support to include common shared bundles

There are probably many improvements to this build and the whole issue of deploying is not presented, but this is a start.

## Structure
The project is structured with a root project containing 2 subtrees. One for common shared bundles and one for the web application. The latter has further division to have the server side and the client side compiled

```
root
 |
 +-- common
 |    |
 |    +-- edu.gemini.seqexec.server
 |
 +-- seqexec-web
      |
      +-- server
      +-- client
      +-- shared
```

Note that `seqexec-web/client` is compiled via `scala.js` and `seqexec-web/shared` is compiled to both `scala.js` and to the JVM

## Versions

Versions are defined in the file `project/Settings.scala`

## Dependencies

All dependencies are managed coming from either maven central or the Gemini repository

Internall, `seqexec-web/server` depends on `common/edu.gemini.seqexec.server`. The dependency is declared on `seqexec-web/build.sbt` but other ways to declare intra-module dependencies could be devised

## Running
On `seqexec-web/server` there is a standalone application that will launch a web server and the Seqexec server. All the static files and generated java script are served from here

To launch you can do:

```
sbt
project seqexec_web_server
~re-start
```

Note that as part of this run command the scala.js application will be compiled to javascript. Modifying any file on server or client will make a restart of the server

## Testing

Sample tests using `scalatest` and `scalacheck` are included in each module. You can run `test` at the root level or per sub-project

Note that IDEA doesn't support running tests on scala.js. See this [issue](https://github.com/scalatest/scalatest/issues/743)

Testing of js code can be done on the browser with Selenium, running

```
firefox:test
```
or
```
chrome:test
```

## IDEA

IDEA can open directly the projepct's sbt. Go to 

```
File -> New -> Project from Existing Sources...
```

And open the root build.sbt. IDEA shows some warnings but the final result works just fine. Note that some files
