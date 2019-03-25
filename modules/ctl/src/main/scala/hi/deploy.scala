// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ctl.hi

import cats.implicits._
import gem.ctl.free.ctl._
import gem.ctl.low.docker._

object Deploy {

  final case class Deployment(gem: Container, postgres: Container)

  private def destroyDeployment(newGemImage: Image): CtlIO[Unit] =
    gosub("Destroying current deployment, if any.") {
      currentDeployment.flatMap {
        case None => info("None found, nothing to do!")
        case Some(Deployment(g, p)) =>
          containerImage(g).flatMap { oldGemImage =>
            destroyContainer(g, !(oldGemImage like newGemImage)) *> destroyContainer(p, false)
          }
      }
    }

  private def deployDatabase(version: String, iPg: Image, n: Network): CtlIO[Container] =
    gosub("Deploying postgres.") {
      for {
        kDb <- Containers.createDatabaseContainer(version, iPg, n)
        _   <- Postgres.createDatabase(version, kDb)
        _   <- Containers.awaitHealthy(kDb)
      } yield kDb
    }

  private def deployGem(version: String, iGem: Image, n: Network): CtlIO[Container] =
    gosub("Deploying Gem.") {
      for {
        kGem <- Containers.createGemContainer(version, iGem, n)
        // _    <- awaitHealthy(kGem)
      } yield kGem
    }

  private def currentDeployment: CtlIO[Option[Deployment]] =
    gosub("Finding current deployment.") {
      findRunningContainersWithLabel("gem.version").flatMap {
        case Nil          => info("No current deployment.").as(None)
        case List(c1, c2) =>
          for {
            r1 <- getLabelValue("gem.role", c1)
            r2 <- getLabelValue("gem.role", c2)
            d  <- (r1, r2) match {
              case ("gem", "db") => Deployment(c1, c2).pure[CtlIO]
              case ("db", "gem") => Deployment(c2, c1).pure[CtlIO]
              case p             => error(s"Unexpected 'gem.role' labels: $p") *> exit[Deployment](-1)
            }
            _  <- info(s"Current Gem container is ${d.gem.hash}.")
            _  <- info(s"Current Postgres container is ${d.postgres.hash}.")
          } yield Some(d)
        case cs => error(s"Expected exactly zero or two containers; found ${cs.length}.") *> exit(-1)
      }
    }

  private def getHistory(i: Image): CtlIO[List[String]] =
    readFileFromImage(i, "/opt/docker/GIT_HISTORY")

  private def verifyCompatibility(curr: Container, next: Image): CtlIO[Unit] =
    gosub("Verifying upgrade compatibility.") {
      for {
        _  <- info(s"Current gem container is ${curr.hash}")
        _  <- info(s"New gem image is ${next.hash}")
        c  <- getLabelValue("gem.commit", curr)
        cs <- getHistory(next)
        _  <- if (cs.contains(c)) info("They are compatible. The schema can be upgraded.")
              else error(s"New deployment is incompatible; missing commit: $c") *> exit(-1)
      } yield ()
    }

  private def verifyNewVersion(current: Deployment, newVersion: String): CtlIO[Unit] =
    getLabelValue("gem.version", current.gem).flatMap {
      case `newVersion` => info("The requested version is already deployed here. Nothing to do.") *> exit(0)
      case _            => ().pure[CtlIO]
    }

  def deployTest(version: String): CtlIO[Deployment] =
    for {
      h  <- serverHostName
      _  <- info(s"This is a test deployment on $h. Any existing deployment will be destroyed!")
      n  <- Networks.getPrivateNetwork
      gi <- Images.getGemImage(version)
      pi <- Images.getPostgresImage(gi)
      _  <- destroyDeployment(gi)
      pk <- deployDatabase(version, pi, n)
      gk <- deployGem(version, gi, n)
    } yield Deployment(gk, pk)

  def deployProduction(version: String): CtlIO[Deployment] =
    for {
      h  <- serverHostName
      _  <- info(s"This is a production upgrade on $h.")
      n  <- Networks.getPrivateNetwork
      gi <- Images.getGemImage(version)
      pi <- Images.getPostgresImage(gi)
      od <- currentDeployment
      d  <- od.fold(error("No current deployment. Use deploy-test to bootstrap.") *> exit[Deployment](-1))(_.pure[CtlIO])
      _  <- verifyNewVersion(d, version)
      _  <- verifyCompatibility(d.gem, gi)
      pk <- deployDatabase(version, pi, n)
      ms <- now
      _  <- info("Stopping Gem container. Downtime starting now.") *> stopContainer(d.gem)
      _  <- Postgres.copyData(d.postgres, pk)
      _  <- stopContainer(d.postgres)
      gk <- deployGem(version, gi, n)
      dt <- now.map(_ - ms)
      _  <- info(s"Upgrade successful. Total downtime ${dt / 1000} seconds.")
    } yield Deployment(gk, pk)

}
