# Seqexec Web Shared

This bundle contains code that need to be used by both the client and the server bundle. As such it needs to be compiled for both the JVM and JS.

## Cross compilation

The recommended way with a shared package is to use the `crossProject` feature of the sbt scala.js plugin, see [Cross-Compilation](http://www.scala-js.org/doc/project/cross-build.html).

## Testing

Tests are done using `ScalaTest` and should be executed in both backends for completness	