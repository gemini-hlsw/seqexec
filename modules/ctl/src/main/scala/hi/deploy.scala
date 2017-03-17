package gem.ctl.hi

import gem.ctl.free.ctl._
import gem.ctl.low.io._
import gem.ctl.low.git._
import gem.ctl.low.docker._
import gem.ctl.hi.common._

import scalaz._, Scalaz._

/** Constructors for `CtlIO` operations related to the `deploy` command. */
object deploy {

  val PrivateNetwork = "gem-net"
  val GemOrg         = "geminihlsw"
  val GemImage       = "gem-telnetd"
  val GemProject     = "telnetd"
  val DeployTagRegex = "^deploy-\\d+$".r
  val Port           = 1234

  def getNetwork: CtlIO[Network] =
    gosub(s"Verifying $PrivateNetwork network.") {
      findNetwork(PrivateNetwork).flatMap {
        case Some(n) => info(s"Using existing network ${n.hash}.").as(n)
        case None    => createNetwork(PrivateNetwork) >>! { n => info(s"Created network ${n.hash}.") }
      }
    }

  def getDeployCommit(rev: String): CtlIO[DeployCommit] =
    gosub("Verifying deploy commit.") {
      for {
        c <- info(s"Using $rev.") *> commitForRevision(rev)
        _ <- info(s"Commit is ${c.hash}")
        u <- uncommittedChanges.map(_ && rev == "HEAD")
        _ <- u.whenM(warn("There are uncommitted changes. This is a UNCOMMITTED deployment."))
      } yield DeployCommit(c, u)
    }

  def getDeployImage(dc: DeployCommit): CtlIO[Image] =
    gosub("Verifying Gem deploy image.") {
      requireImage(s"$GemOrg/$GemImage:${dc.imageVersion}")
    }

  def fileContentsAtDeployCommit(cDeploy: DeployCommit, path: String): CtlIO[List[String]] =
    if (cDeploy.uncommitted) fileContents(path)
    else fileContentsAtCommit(cDeploy.commit, path)

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

  def getPostgresImage(cDeploy: DeployCommit): CtlIO[Image] =
    gosub("Verifying Postgres deploy image.") {
      fileContentsAtDeployCommit(cDeploy, "pg-image.txt").flatMap { ss =>
        ss.map(_.trim).filterNot(s => s.startsWith("#") || s.trim.isEmpty) match {
          case List(name) => requireImage(name)
          case _          =>
            error(s"Cannot determine Postgres image for deploy commit $cDeploy") *>
            exit(-1)
        }
      }
    }

  def verifyLineage(base: DeployCommit, dc: DeployCommit): CtlIO[Unit] =
    gosub("Verifying lineage.") {
      (base, dc) match {
        case (DeployCommit(b, true),  DeployCommit(d, true))  if b === d => info("Base and deploy commits are matching and UNCOMMITTED. All is well.")
        case (DeployCommit(b, false), DeployCommit(d, false)) if b === d => info("Base and deploy commits are identical. Nothing to do.") *> exit(0)
        case (DeployCommit(b, _),     DeployCommit(d, _)) =>
          isAncestor(b, d).flatMap {
            case true  => info( s"Base commit is an ancestor of deploy commit (as it should be).")
            case false => error(s"Base commit is not an ancestor of deploy commit.") *> exit[Unit](-1)
          }
      }
    }

  def ensureNoRunningDeployments: CtlIO[Unit] =
    gosub("Ensuring that there is no running deployment.") {
      findRunningContainersWithLabel("edu.gemini.commit").flatMap {
        case Nil => info("There are no running deployments.")
        case cs  =>
          for {
            c <- config
            _ <- error(s"There is already a running deployment on ${c.server}")
            _ <- exit[Unit](-1)
          } yield ()
      }
    }

  def createDatabaseContainer(nDeploy: Int, cDeploy: DeployCommit, iPg: Image): CtlIO[Container] =
    gosub(s"Creating Postgres container from image ${iPg.hash}") {
      for {
        c <- docker("run",
                  "--detach",
                 s"--net=$PrivateNetwork",
                  "--name", s"db-$nDeploy",
                  "--label", s"edu.gemini.commit=${cDeploy.imageVersion}",
                  "--label",  "edu.gemini.db=true",
                  "--health-cmd", "\"psql -U postgres -d gem -c 'select 1'\"",
                  "--health-interval", "10s",
                  "--health-retries", "2",
                  "--health-timeout", "2s",
                  iPg.hash
             ).require { case Output(0, List(s)) => Container(s) }
        _ <- info(s"Container is db-${nDeploy} ${c.hash}.")
      } yield c
    }

  def getDeployNumber: CtlIO[Int] =
    for {
      n <- allContainerNames.map { ss =>
             ss.map(_.split("\\-")).collect {
               case Array("db", s)  => s
               case Array("gem", s) => s
               case _               => ""
             } .map(_.parseInt.toOption.getOrElse(0)) .foldLeft(0)(_ max _) + 1
           }
      _ <- info(s"Deploy number for this host will be $n.")
    } yield n

  def createDatabase(nDeploy: Int, kPg: Container): CtlIO[Unit] =
    for {
      b <- docker("exec", kPg.hash,
             "psql", s"--host=db-$nDeploy",
                      "--command='create database gem'",
                      "--username=postgres"
           ).require {
             case Output(0, List("CREATE DATABASE")) => true
             case Output(2, _)                       => false
           }
      _ <- if (b) info("Created database.")
           else {
             info(s"Waiting for Postgres to start up.") *>
             shell("sleep 2") *>
             createDatabase(nDeploy, kPg)
           }
    } yield ()

  def awaitHealthy(k: Container): CtlIO[Unit] =
    containerHealth(k) >>= {
      case "starting" => info( s"Waiting for health check.") *> shell("sleep 2") *> awaitHealthy(k)
      case "healthy"  => info( s"Container is healthy.")
      case s          => error(s"Health check failed: $s") *> exit(-1)
    }

  def deployDatabase(nDeploy: Int, cDeploy: DeployCommit, iPg: Image): CtlIO[Container] =
    gosub("Deploying database.") {
      for {
        kDb <- createDatabaseContainer(nDeploy, cDeploy, iPg)
        _   <- createDatabase(nDeploy, kDb)
        _   <- awaitHealthy(kDb)
      } yield kDb
    }

  def createGemContainer(nDeploy: Int, cDeploy: DeployCommit, iDeploy: Image) =
    gosub(s"Creating Gem container from image ${iDeploy.hash}") {
      for {
        k  <- docker("run",
                "--detach",
                "--tty",
                "--interactive",
               s"--net=$PrivateNetwork",
                "--name",    s"gem-$nDeploy",
                "--label",   s"edu.gemini.commit=${cDeploy.imageVersion}",
                "--label",    "edu.gemini.gem=true",
                "--publish", s"$Port:6666",
                "--env",     s"GEM_DB_URL=jdbc:postgresql://db-$nDeploy/gem",
                iDeploy.hash
              ).require { case Output(0, List(s)) => Container(s) }
        _  <- info(s"Container is gem-${nDeploy} ${k.hash}.")
      } yield k
    }

  def awaitNet(host: String, port: Int, retries: Int = 9): CtlIO[Unit] =
    retries match {
      case 0 => error("Remote port is unavailable. Hm.") *> exit(-1)
      case n => shell("nc", "-z", host, port.toString).require {
        case Output(0, _) => true
        case Output(1, _) => false
      } flatMap {
        case true  => info(s"Service is available at $host:$port.")
        case false =>
          info(s"Awaiting port availability (remaining retries: $n)") *>
          shell("sleep 2") *> awaitNet(host, port, n - 1)
      }
    }

  def deployGem(nDeploy: Int, cDeploy: DeployCommit, iDeploy: Image) =
    gosub("Deploying Gem.") {
      for {
        kGem <- createGemContainer(nDeploy, cDeploy, iDeploy)
        h    <- serverHostName
        _    <- awaitNet(h, Port)
      } yield kGem
    }

  def deployStandalone(nDeploy: Int, cDeploy: DeployCommit, iDeploy: Image, iPg: Image) =
    gosub("Performing STANDALONE deployment") {
      for {
        _   <- ensureNoRunningDeployments
        kDb <- deployDatabase(nDeploy, cDeploy, iPg)
        _   <- deployGem(nDeploy, cDeploy, iDeploy)
      } yield ()
    }

  def copyData(fromName: String, toContainer: Container): CtlIO[Unit] =
    gosub(s"Copying data from $fromName") {
      docker(
        "exec", toContainer.hash,
        "sh", "-c", s"'pg_dump -h $fromName -U postgres gem | psql -q -U postgres -d gem'"
      ) require {
        case Output(0, _) => ()
      }
    }

  def deployUpgrade(nDeploy: Int, cDeploy: DeployCommit, iDeploy: Image, iPg: Image) =
    gosub("Performing UPGRADE deployment") {
      for {
        kGem  <- getRunningGemContainer
        cBase <- getDeployCommitForContainer(kGem)
        _     <- verifyLineage(cBase, cDeploy)
        kPg   <- getRunningPostgresContainer
        cPg   <- getDeployCommitForContainer(kPg)
        _     <- if (cPg.commit === cBase.commit) info( "Gem and Postgres containers are at the same commit.")
                 else                             error("Gem and Postgres containers are at different commits. What?") *> exit(-1)
        _     <- gosub("Stopping old Gem container.")(stopContainer(kGem))
        kPg2  <- deployDatabase(nDeploy, cDeploy, iPg)
        nPg   <- getContainerName(kPg)
        _     <- copyData(nPg, kPg2)
        _     <- gosub("Stopping old database container.")(stopContainer(kPg))
        _     <- deployGem(nDeploy, cDeploy, iDeploy)
      } yield ()
    }

  def deploy(deploy: String, standalone: Boolean) =
    for {
      nDeploy  <- getDeployNumber
      cDeploy  <- getDeployCommit(deploy)
      iPg      <- getPostgresImage(cDeploy)
      iDeploy  <- getDeployImage(cDeploy)
      network  <- getNetwork
      _        <- if (standalone) deployStandalone(nDeploy, cDeploy, iDeploy, iPg)
                  else            deployUpgrade(   nDeploy, cDeploy, iDeploy, iPg)
    } yield ()

}
