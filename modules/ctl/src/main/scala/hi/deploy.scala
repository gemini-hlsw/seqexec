// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ctl.hi

import cats.implicits._
import gem.ctl.free.ctl._
import gem.ctl.low.docker._

final case class Deployment(gem: Container, postgres: Container)

/** Constructors for `CtlIO` operations related to the `deploy` command. */
object Deploy {

  def destroyDeployment: CtlIO[Unit] =
    gosub("Destroying current deployment, if any.") {
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

  def currentDeployment: CtlIO[Option[Deployment]] =
    gosub("Finding current deployment.") {
      findRunningContainersWithLabel("gem.version").flatMap {
        case Nil          => info("No current deployment.").as(None)
        case List(c1, c2) =>
          for {
            r1 <- getLabelValue("gem.role", c1)
            r2 <- getLabelValue("gem.role", c2)
            d  <- (r1, r2) match {
              case ("gem", "db") => Some(Deployment(c1, c2)).pure[CtlIO]
              case ("db", "gem") => Some(Deployment(c2, c1)).pure[CtlIO]
              case p             => error(s"Unexpected 'gem.role' labels: $p") *> exit[Option[Deployment]](-1)
            }
            _  <- info(s"Found current deployment: $d")
          } yield d
        case cs => error(s"Expected exactly zero or two containers; found ${cs.length}.") *> exit(-1)
      }
    }

  def deployTest(version: String): CtlIO[Deployment] =
    for {
      h  <- serverHostName
      _  <- info(s"This is a test deployment on $h. Any existing deployment will be destroyed!")
      n  <- Networks.getPrivateNetwork
      gi <- Images.getGemImage(version)
      pi <- Images.getPostgresImage(gi)
      d  <- currentDeployment
      _  <- exit[Int](-1)
      _  <- destroyDeployment
      pk <- deployDatabase(version, pi, n)
      gk <- deployGem(version, gi, n)
    } yield Deployment(gk, pk)


  // def deployProduction(version: String): CtlIO[Deployment] =
  //   for {
  //     h  <- serverHostName
  //     _  <- info(s"This is a production upgrade on $h.")
  //     n  <- Networks.getPrivateNetwork
  //     gi <- Images.getGemImage(version)
  //     pi <- Images.getPostgresImage(gi)
  //     d  <- currentDeployment
  //     // verify upgrade path
  //     pk <- deployDatabase(version, pi, n) // specify prior container
  //     // stop d.gem
  //     // log "production system is down. current time is ..."
  //     // stream data from d.postgres
  //     // shut down d.postgres
  //     gk <- deployGem(version, gi, n) // specify prior container
  //     // log "upgrade successful, total downtime:..."
  //   } yield Deployment(gk, pk)

}
