// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ctl.hi

import gem.ctl.free.ctl._
import gem.ctl.low.io._
// import gem.ctl.low.git._
import gem.ctl.low.docker._
// import gem.ctl.hi.common._

import cats.implicits._
// import scala.util.matching.Regex
// import mouse.all._

/** Constructors for `CtlIO` operations related to the `deploy` command. */
object deploy2 {

  val PrivateNetwork: String  = "gem-net"
  val GemRegistry: String     = "sbfocsdev-lv1.cl.gemini.edu"
  val GemImage: String        = "gem"
  // val Port: Int               = 1234
  // val awaitNetRetries: Int    = 9
  //
  def getNetwork: CtlIO[Network] =
    gosub(s"Verifying $PrivateNetwork network.") {
      findNetwork(PrivateNetwork).flatMap {
        case Some(n) => info(s"Using existing network ${n.hash}.").as(n)
        case None    =>
          for {
            n <- createNetwork(PrivateNetwork)
            _ <- info(s"Created network ${n.hash}.")
          } yield n
      }
    }

  // def getDeployCommit(rev: String): CtlIO[DeployCommit] =
  //   gosub("Verifying deploy commit.") {
  //     for {
  //       c <- info(s"Using $rev.") *> commitForRevision(rev)
  //       _ <- info(s"Commit is ${c.hash}")
  //       u <- uncommittedChanges.map(_ && rev === "HEAD")
  //       _ <- if (u) warn("There are uncommitted changes. This is a UNCOMMITTED deployment.") else ().pure[CtlIO]
  //     } yield DeployCommit(c, u)
  //   }

  def getDeployImage(version: String): CtlIO[Image] =
    gosub("Setting up Gem image.") {
      for {
        i <- requireImage(s"$GemRegistry/$GemImage:$version")
        _ <- ensureImageLabel("gem.version", version, i)
        _ <- ensureImageLabel("gem.unstable", false.toString, i)
      } yield i
    }

  // def fileContentsAtDeployCommit(cDeploy: DeployCommit, path: String): CtlIO[List[String]] =
  //   if (cDeploy.uncommitted) fileContents(path)
  //   else fileContentsAtCommit(cDeploy.commit, path)
  //
  def requireImage(nameAndVersion: String): CtlIO[Image] = {
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

  def getPostgresImage(gi: Image): CtlIO[Image] =
    gosub("Setting up Postgres image.") {
      getImageLabel("gem.postgres", gi).flatMap(requireImage)
    }

  // def verifyLineage(base: DeployCommit, dc: DeployCommit, force: Boolean): CtlIO[Unit] =
  //   gosub("Verifying lineage.") {
  //     (base, dc) match {
  //       case (DeployCommit(b, true),  DeployCommit(d, true))  if b === d => info("Base and deploy commits are matching and UNCOMMITTED. All is well.")
  //       case (DeployCommit(b, false), DeployCommit(d, false)) if b === d =>
  //         info("Base and deploy commits are identical.") *> {
  //           if (force) info("--force was specified, so upgrading anyway.")
  //           else       info("Nothing to do. Use --force to do upgrade anyway.") *> exit(0)
  //         }
  //       case (DeployCommit(b, _),     DeployCommit(d, _)) =>
  //         isAncestor(b, d).flatMap {
  //           case true  => info( s"Base commit is an ancestor of deploy commit (as it should be).")
  //           case false => error(s"Base commit is not an ancestor of deploy commit.") *> exit[Unit](-1)
  //         }
  //     }
  //   }

  def stopDeployment: CtlIO[Unit] =
    gosub("Stopping current deployment, if any.") {
      findRunningContainersWithLabel("gem.version").flatMap {
        case Nil => info("None found, nothing to do!")
        case cs  => cs.traverse { c =>
          info(s"TODO: get container name ... stopping ${c.hash}") *> stopContainer(c)
        }.void
      }
    }

  def createDatabaseContainer(version: String, iPg: Image): CtlIO[Container] =
    gosub(s"Creating Postgres container from image ${iPg.hash}") {
      for {
        r <- isRemote // irritating … see below
        c <- docker("run",
                  "--detach",
                 s"--net=$PrivateNetwork",
                  "--name", s"$version-P",
                  "--label", s"gem.version=$version",
                  "--health-cmd", if (r) "\"psql -U postgres -d gem -c 'select 1'\""
                                  else     "psql -U postgres -d gem -c 'select 1'",
                  "--health-interval", "10s",
                  "--health-retries", "2",
                  "--health-timeout", "2s",
                  iPg.hash
             ).require { case Output(0, List(s)) => Container(s) }
        _ <- info(s"Container is $version-P ${c.hash}.")
      } yield c
    }

  // def getDeployNumber: CtlIO[Int] =
  //   for {
  //     n <- allContainerNames.map { ss =>
  //            ss.map(_.split("\\-")).collect {
  //              case Array("db", s)  => s
  //              case Array("gem", s) => s
  //              case _               => ""
  //            } .map(_.parseInt.toOption.getOrElse(0)) .foldLeft(0)(_ max _) + 1
  //          }
  //     _ <- info(s"Deploy number for this host will be $n.")
  //   } yield n

  def createDatabase(version: String, kPg: Container): CtlIO[Unit] =
    for {
      r <- isRemote // irritating … see below
      b <- docker("exec", kPg.hash,
             "psql", s"--host=$version-P",
                     if (r) "--command='create database gem'"
                     else   "--command=create database gem",
                      "--username=postgres"
           ).require {
             case Output(0, List("CREATE DATABASE")) => true
             case Output(2, _)                       => false
           }
      _ <- if (b) info("Created database.")
           else {
             info(s"Waiting for Postgres to start up.") *>
             shell("sleep", "2") *>
             createDatabase(version, kPg)
           }
    } yield ()

  def awaitHealthy(k: Container): CtlIO[Unit] =
    containerHealth(k) >>= {
      case "starting" => info( s"Waiting for health check.") *> shell("sleep", "2") *> awaitHealthy(k)
      case "healthy"  => info( s"Container is healthy.")
      case s          => error(s"Health check failed: $s") *> exit(-1)
    }

  def deployDatabase(version: String, iPg: Image): CtlIO[Container] =
    gosub("Deploying postgres.") {
      for {
        kDb <- createDatabaseContainer(version, iPg)
        _   <- createDatabase(version, kDb)
        _   <- awaitHealthy(kDb)
      } yield kDb
    }

  // def createGemContainer(nDeploy: Int, cDeploy: DeployCommit, iDeploy: Image, kPrev: Option[Container]): CtlIO[Container] =
  //   gosub(s"Creating Gem container from image ${iDeploy.hash}") {
  //     for {
  //       k  <- docker("run",
  //               "--detach",
  //               "--tty",
  //               "--interactive",
  //              s"--net=$PrivateNetwork",
  //               "--name",    s"gem-$nDeploy",
  //               "--label",   s"edu.gemini.commit=${cDeploy.imageVersion}",
  //               "--label",    "edu.gemini.gem=true",
  //               "--label",   s"edu.gemini.prev=${kPrev.fold("")(_.hash)}",
  //               "--publish", s"$Port:6666",
  //               "--env",     s"GEM_DB_URL=jdbc:postgresql://db-$nDeploy/gem",
  //               iDeploy.hash
  //             ).require { case Output(0, List(s)) => Container(s) }
  //       _  <- info(s"Container is gem-${nDeploy} ${k.hash}.")
  //     } yield k
  //   }
  //
  // def awaitNet(host: String, port: Int, retries: Int): CtlIO[Unit] =
  //   retries match {
  //     case 0 => error("Remote port is unavailable. Hm.") *> exit(-1)
  //     case n => shell("nc", "-z", host, port.toString).require {
  //       case Output(0, _) => true
  //       case Output(1, _) => false
  //     } flatMap {
  //       case true  => info(s"Service is available at $host:$port.")
  //       case false =>
  //         info(s"Awaiting port availability (remaining retries: $n)") *>
  //         shell("sleep", "2") *> awaitNet(host, port, n - 1)
  //     }
  //   }
  //
  // def deployGem(nDeploy: Int, cDeploy: DeployCommit, iDeploy: Image, kPrev: Option[Container]): CtlIO[Container] =
  //   gosub("Deploying Gem.") {
  //     for {
  //       kGem <- createGemContainer(nDeploy, cDeploy, iDeploy, kPrev)
  //       h    <- serverHostName
  //       _    <- awaitNet(h, Port, awaitNetRetries)
  //     } yield kGem
  //   }
  //
  // def copyData(fromName: String, toContainer: Container): CtlIO[Unit] =
  //   gosub(s"Copying data from $fromName") {
  //     isRemote.flatMap { r =>
  //       docker(
  //         "exec", toContainer.hash,
  //         "sh", "-c", if (r) s"'pg_dump -h $fromName -U postgres gem | psql -q -U postgres -d gem'"
  //                     else    s"pg_dump -h $fromName -U postgres gem | psql -q -U postgres -d gem"
  //       ) require {
  //         case Output(0, _) => ()
  //       }
  //     }
  //   }
  //
  // def deployStandalone(nDeploy: Int, cDeploy: DeployCommit, iDeploy: Image, iPg: Image): CtlIO[Unit] =
  //   gosub("Performing STANDALONE deployment") {
  //     for {
  //       _   <- stopDeployment
  //       kDb <- deployDatabase(nDeploy, cDeploy, iPg, None)
  //       _   <- deployGem(nDeploy, cDeploy, iDeploy, None)
  //     } yield ()
  //   }
  //
  // def deployUpgrade(nDeploy: Int, cDeploy: DeployCommit, iDeploy: Image, iPg: Image, force: Boolean): CtlIO[Unit] =
  //   gosub("Performing UPGRADE deployment") {
  //     for {
  //       kGem  <- getRunningGemContainer
  //       cBase <- getDeployCommitForContainer(kGem)
  //       _     <- verifyLineage(cBase, cDeploy, force)
  //       kPg   <- getRunningPostgresContainer
  //       cPg   <- getDeployCommitForContainer(kPg)
  //       _     <- if (cPg.commit === cBase.commit) info( "Gem and Postgres containers are at the same commit.")
  //                else                             error("Gem and Postgres containers are at different commits. What?") *> exit(-1)
  //       _     <- gosub("Stopping old Gem container.")(stopContainer(kGem))
  //       kPg2  <- deployDatabase(nDeploy, cDeploy, iPg, Some(kPg))
  //       nPg   <- getContainerName(kPg)
  //       _     <- copyData(nPg, kPg2)
  //       _     <- gosub("Stopping old database container.")(stopContainer(kPg))
  //       _     <- deployGem(nDeploy, cDeploy, iDeploy, Some(kGem))
  //     } yield ()
  //   }

  def deploy2(version: String): CtlIO[Unit] =
    for {
      _  <- info("This is a TEST deployment.")
      _  <- getNetwork
      gi <- getDeployImage(version)
      pi <- getPostgresImage(gi)
      _  <- stopDeployment
      pk <- deployDatabase(version, pi)
    } yield ()

}
