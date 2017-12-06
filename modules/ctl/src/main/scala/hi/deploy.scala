// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ctl.hi

import cats.implicits._
import gem.ctl.free.ctl._
import gem.ctl.low.docker._

/** Constructors for `CtlIO` operations related to the `deploy` command. */
object Deploy {

  def stopDeployment: CtlIO[Unit] =
    gosub("Stopping current deployment, if any.") {
      findRunningContainersWithLabel("gem.version").flatMap {
        case Nil => info("None found, nothing to do!")
        case cs  => cs.traverse { c =>
          info(s"Stopping and removing ${c.hash}.") *> destroyContainer(c)
        }.void
      }
    }

  def deployDatabase(version: String, iPg: Image, n: Network): CtlIO[Container] =
    gosub("Deploying postgres.") {
      for {
        kDb <- Containers.createDatabaseContainer(version, iPg, n)
        _   <- Postgres.createDatabase(version, kDb)
        _   <- Containers.awaitHealthy(kDb)
      } yield kDb
    }

  def deployGem(version: String, iGem: Image, n: Network): CtlIO[Container] =
    gosub("Deploying Gem.") {
      for {
        kGem <- Containers.createGemContainer(version, iGem, n)
        // _    <- awaitHealthy(kGem)
      } yield kGem
    }

  def deployTest(version: String): CtlIO[Unit] =
    for {
      h  <- serverHostName
      _  <- info(s"This is a test deployment on $h. Any existing deployment will be destroyed!")
      n  <- Networks.getPrivateNetwork
      gi <- Images.getDeployImage(version)
      pi <- Images.getPostgresImage(gi)
      _  <- stopDeployment
      pk <- deployDatabase(version, pi, n)
      gk <- deployGem(version, gi, n)
    } yield ()

}
