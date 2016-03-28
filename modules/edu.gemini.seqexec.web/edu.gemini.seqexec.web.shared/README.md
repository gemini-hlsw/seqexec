# Seqexec Web Shared

This bundle contains code that need to be used by both the client and the server bundle. As such it needs to be compiled for both the JVM and JS.

## Cross compilation

The recommended way with a shared package is to use the `crossProject` feature of the sbt scala.js plugin, see [Cross-Compilation](http://www.scala-js.org/doc/project/cross-build.html). However, I found this method to be not compatible with the structure of the OCS build. An alternative was implemente where the shared src directory is included for compilation in both client and server bundles achieving the same effect.

Note that in this bundle we are using scalaz and the build will use the correct one (Either JVM or JS) when it corresponds. We don't have a current use for scalaz on the client, but it was mostly a demonstration of this capability	