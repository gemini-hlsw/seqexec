package gem.ctl.hi

import gem.ctl.free.ctl._
import gem.ctl.low.docker._

import scalaz._, Scalaz._

object stop {

  val stop: CtlIO[Unit] =
    gosub(Info, "Shutting down Gem deployment.",
      for {
        ks <- findRunningContainersWithLabel("edu.gemini.commit")
        _  <- ks.traverseU(stopOne)
      } yield ()
    )

  def stopOne(k: Container): CtlIO[Unit] =
    info(s"Stopping $k") *>
    stopContainer(k)

}
