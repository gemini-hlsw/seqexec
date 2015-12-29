import OcsKeys._

// note: inter-project dependencies are declared at the top, in projects.sbt

name := "edu.gemini.seqexec.shared"

// version set in ThisBuild

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.0.6")

osgiSettings

ocsBundleSettings

OsgiKeys.bundleActivator := Some("edu.gemini.seqexec.shared.osgi.Activator")

OsgiKeys.bundleSymbolicName := name.value

OsgiKeys.dynamicImportPackage := Seq("")

OsgiKeys.exportPackage := Seq(
  "edu.gemini.seqexec.shared")
