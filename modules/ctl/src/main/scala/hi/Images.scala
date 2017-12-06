// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ctl.hi

import cats.implicits._
import gem.ctl.free.ctl._
import gem.ctl.low.docker._

object Images {

  private val GemRegistry: String = "sbfocsdev-lv1.cl.gemini.edu"
  private val GemImage: String    = "gem"

  private def requireImage(nameAndVersion: String): CtlIO[Image] = {
    info(s"Looking for $nameAndVersion") *>
    findImage(nameAndVersion).flatMap {
      case None    =>
        warn(s"Cannot find image locally. Pulling (could take a few minutes).") *> pullImage(nameAndVersion).flatMap {
          case None    => error(s"Image was not found.") *> exit(-1)
          case Some(i) => info(s"Image is ${i.hash}") as i
        }
      case Some(i) => info(s"Image is ${i.hash}") as i
    }
  }

  def getDeployImage(version: String): CtlIO[Image] =
    gosub("Setting up Gem image.") {
      for {
        i <- requireImage(s"$GemRegistry/$GemImage:$version")
        _ <- ensureImageLabel("gem.version", version, i)
        r <- isRemote
        _ <- ensureImageLabel("gem.unstable", false.toString, i).whenA(r)
      } yield i
    }

  def getPostgresImage(gi: Image): CtlIO[Image] =
    gosub("Setting up Postgres image.") {
      for {
        n <- getImageLabel("gem.postgres", gi)
        _ <- info(s"Gem image requires $n")
        i <- requireImage(n)
      } yield i
    }

}
