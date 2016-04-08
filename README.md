# OCS3

This project aims to build the next generation Observatory Control System (OCS). At the time of this writing, only the Seqexec has been integrated. This setup has the following goals 

- Adhere to the simplest possible sbt structure.
- No need for OSGi
- Use managed dependencies
- Support of web applications using scala.js
- Testing supported on both JVM and JS backends

Some build-level tasks are still pending

- [ ] Support deployable artifacts
- [ ] Include modules cross-compiled to both JVM and JS backends
- [ ] Enable travis
- [ ] Unify JVM/JS scalaz versions

## Structure
The project contains a single `modules` subtree. Inside that module each of the bundles is located. Web applications are further devided internally to have the server side, client side and shared code compiled to their respective backends

```
ocs3/
 |
 +-- modules/
 |    |
 |    +-- edu.gemini.seqexec.server/
 |    +-- edu.gemini.seqexec.shared/
 |    +-- edu.gemini.seqexec.web/
 |           |
 |           +-- edu.gemini.seqexec.web.shared/
 |           +-- edu.gemini.seqexec.web.server/
 |           +-- edu.gemini.seqexec.web.client/
 +-- lib/
 +-- project/
```
`lib` contains only unmanaged dependencies where `project` includes sbt definitions

## Settings

Settings and module versions are defined in the file `project/Settings.scala`

## Dependencies

Most dependencies are managed dependencies coming from either maven central or the Gemini repository.

Intra-module dependencies are declared on `project/OcsBuild`. Being at the top level the project declarations can be used on each module's `build.sbt`

## Testing

`ScalaTest` has been chosen as the test framework since it works in both scala.js and jvm. `ScalaCheck` is also supported running with `ScalaTest`. Sample tests using `scalatest` and `scalacheck` are included in some modules. You can run `test` at the root level or on a module basis.

Note that IDEA doesn't support running tests on scala.js. See this [issue](https://github.com/scalatest/scalatest/issues/743)

## IDEA

Before opening the project in IDEA, run a top-level compile at least once to generate code that will be picked up by IDEA

IDEA can open directly the project's sbt. Go to 

```
File -> Open
```

And find the file `ocs/build.sbt`. IDEA should show a dialog box to import the project. You may need to select the appropriate JVM version (1.8) and IDEA will import the project. `sbt` files may have highlight errors due to this [bug](https://youtrack.jetbrains.com/issue/SCL-9599). Otherwise, sbt import seems to work quite well.

**Note:** Sbt import in IDEA changes very often. This has been tested on IDEA C 2016.1 and the Scala plugin version 3.0

**Note:** If you need to re-generate the project, delete the `.idea` folder
 