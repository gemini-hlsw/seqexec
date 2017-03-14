package gem.ctl.hi

import gem.ctl.free.ctl._
import gem.ctl.low.io._
import gem.ctl.low.docker._

import scalaz._, Scalaz._

object ps {

  val ps: CtlIO[Unit] =
    for {
      ks <- findRunningContainersWithLabel("edu.gemini.commit")
      _  <- ks.traverseU(psOne)
    } yield ()

  def psOne(k: Container): CtlIO[Unit] =
    docker(
      "inspect",
      "--format", "'{{ index .Config.Labels \"edu.gemini.commit\"}},{{.Name}},{{.State.Status}}'",
      k.hash
    ) require {
      case Output(0, s :: Nil) => s.split(",").toList
    } flatMap {
      case List(c, n, s) => log(Info, s"${k.hash}  $c  $s  $n")
      case ss            => log(Error, s"Bogus response: ${ss.mkString(",")}") *> exit(-1)
    }

}
