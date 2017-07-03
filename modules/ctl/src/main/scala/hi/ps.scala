// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ctl.hi

import gem.ctl.free.ctl._
import gem.ctl.low.io._
import gem.ctl.low.docker._

import cats.implicits._

/** Constructors for `CtlIO` operations related to the `ps` command. */
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
      case List(c, n, s) => info(s"${k.hash}  $c  $s  $n")
      case ss            => error(s"Bogus response: ${ss.mkString(",")}") *> exit(-1)
    }

}
