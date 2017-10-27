// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ctl.hi

import cats.implicits._
import gem.ctl.free.ctl._
import gem.ctl.low.io._
import gem.ctl.low.docker._

/** Constructors for `CtlIO` operations related to the `deploy` command. */
object DeployTest {

  val PrivateNetwork: String  = "gem-net"
  val GemRegistry: String     = "sbfocsdev-lv1.cl.gemini.edu"
  val GemImage: String        = "gem"
  val Port: Int               = 1234
  val awaitNetRetries: Int    = 9

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

  def getDeployImage(version: String): CtlIO[Image] =
    gosub("Setting up Gem image.") {
      for {
        i <- requireImage(s"$GemRegistry/$GemImage:$version")
        _ <- ensureImageLabel("gem.version", version, i)
        _ <- ensureImageLabel("gem.unstable", false.toString, i)
      } yield i
    }

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
      for {
        n <- getImageLabel("gem.postgres", gi)
        _ <- info(s"Gem image requires $n")
        i <- requireImage(n)
      } yield i
    }

  def destroyContainer(k: Container): CtlIO[Unit] =
    stopContainer(k) *> removeContainer(k)

  def stopDeployment: CtlIO[Unit] =
    gosub("Stopping current deployment, if any.") {
      findRunningContainersWithLabel("gem.version").flatMap {
        case Nil => info("None found, nothing to do!")
        case cs  => cs.traverse { c =>
          info(s"Stopping and removing ${c.hash}.") *> destroyContainer(c)
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

  def createGemContainer(version: String, iGem: Image): CtlIO[Container] =
    gosub(s"Creating Gem container from image ${iGem.hash}") {
      for {
        r  <- isRemote // irritating … see below
        k  <- docker("run",
                "--detach",
                "--tty",
                "--interactive",
               s"--net=$PrivateNetwork",
                "--name",    s"$version-G",
                "--label",   s"gem.version=$version",
                // "--health-cmd", if (r) "\"nc -z localhost 6666\""
                //                 else     "nc -z localhost 6666",
                "--publish", s"$Port:6666",
                "--env",     s"GEM_DB_URL=jdbc:postgresql://$version-P/gem",
                iGem.hash
              ).require { case Output(0, List(s)) => Container(s) }
        _ <- info(s"Container is $version-G ${k.hash}.")
      } yield k
    }

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

  def deployGem(version: String, iGem: Image): CtlIO[Container] =
    gosub("Deploying Gem.") {
      for {
        kGem <- createGemContainer(version, iGem)
        // _    <- awaitHealthy(kGem)
      } yield kGem
    }

  def deployTest(version: String): CtlIO[Unit] =
    for {
      h  <- serverHostName
      _  <- info(s"This is an test deployment on $h. Any existing deployment will be destroyed!")
      _  <- getNetwork
      gi <- getDeployImage(version)
      pi <- getPostgresImage(gi)
      _  <- stopDeployment
      pk <- deployDatabase(version, pi)
      gk <- deployGem(version, gi)
    } yield ()

}
