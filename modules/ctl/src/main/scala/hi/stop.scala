// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ctl.hi

import gem.ctl.free.ctl._
import gem.ctl.low.docker._

import cats.implicits._

/** Constructors for `CtlIO` operations related to the `stop` command. */
object stop {

  val stop: CtlIO[Unit] =
    gosub("Shutting down Gem deployment.") {
      for {
        ks <- findRunningContainersWithLabel("edu.gemini.commit")
        _  <- ks.traverse(stopOne)
      } yield ()
    }

  def stopOne(k: Container): CtlIO[Unit] =
    info(s"Stopping $k") *>
    stopContainer(k)

}
