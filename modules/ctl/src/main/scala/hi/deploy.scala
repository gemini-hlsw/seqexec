package gem.ctl.hi

import gem.ctl.free.ctl.{ log => ctlLog, _ }
import gem.ctl.low.io._
import gem.ctl.low.git._
import gem.ctl.low.docker._
import gem.ctl.hi.common._

import scalaz._, Scalaz._

object deploy {

  val PrivateNetwork = "gem-net"
  val GemOrg         = "geminihlsw"
  val GemImage       = "gem-telnetd"
  val GemProject     = "telnetd"
  val DeployTagRegex = "^deploy-\\d+$".r
  val Port           = 1234

  def getNetwork: CtlIO[Network] =
    gosub(Info, s"Verifying $PrivateNetwork network.",
      findNetwork(PrivateNetwork).flatMap {
        case Some(n) => ctlLog(Info, s"Using existing network ${n.hash}.").as(n)
        case None    => createNetwork(PrivateNetwork) >>! { n => ctlLog(Info, s"Created network ${n.hash}.") }
      }
    )

  def getDeployCommit(rev: String): CtlIO[DeployCommit] =
    gosub(Info, "Verifying deploy commit.",
      for {
        c <- ctlLog(Info, s"Using $rev.") *> commitForRevision(rev)
        _ <- ctlLog(Info, s"Commit is ${c.hash}")
        u <- uncommittedChanges.map(_ && rev == "HEAD")
        _ <- u.whenM(ctlLog(Warn, "There are uncommitted changes. This is a UNCOMMITTED deployment."))
      } yield DeployCommit(c, u)
    )

  def getDeployImage(dc: DeployCommit): CtlIO[Image] =
    gosub(Info, "Verifying Gem deploy image.", requireImage(s"$GemOrg/$GemImage:${dc.imageVersion}"))

  def fileContentsAtDeployCommit(cDeploy: DeployCommit, path: String): CtlIO[List[String]] =
    if (cDeploy.uncommitted) fileContents(path)
    else fileContentsAtCommit(cDeploy.commit, path)

  def requireImage(nameAndVersion: String): CtlIO[Image] = {
    ctlLog(Info, s"Looking for $nameAndVersion") *>
    findImage(nameAndVersion).flatMap {
      case None    =>
        ctlLog(Warn, s"Cannot find image locally. Pulling (could take a few minutes).") *> pullImage(nameAndVersion).flatMap {
          case None    => ctlLog(Error, s"Image was not found.") *> exit(-1)
          case Some(i) => ctlLog(Info, s"Image is ${i.hash}") as i
        }
      case Some(i) => ctlLog(Info, s"Image is ${i.hash}") as i
    }
  }

  def getPostgresImage(cDeploy: DeployCommit): CtlIO[Image] =
    gosub(Info, "Verifying Postgres deploy image.",
      fileContentsAtDeployCommit(cDeploy, "pg-image.txt").flatMap { ss =>
        ss.map(_.trim).filterNot(s => s.startsWith("#") || s.trim.isEmpty) match {
          case List(name) => requireImage(name)
          case _          =>
            ctlLog(Error, s"Cannot determine Postgres image for deploy commit $cDeploy") *>
            exit(-1)
        }
      }
    )

  def verifyLineage(base: DeployCommit, dc: DeployCommit): CtlIO[Unit] =
    gosub(Info, "Verifying lineage.",
      (base, dc) match {
        case (DeployCommit(b, true),  DeployCommit(d, true))  if b === d => ctlLog(Info, "Base and deploy commits are matching and UNCOMMITTED. All is well.")
        case (DeployCommit(b, false), DeployCommit(d, false)) if b === d => ctlLog(Info, "Base and deploy commits are identical. Nothing to do.") *> exit(0)
        case (DeployCommit(b, _),     DeployCommit(d, _)) =>
          isAncestor(b, d).flatMap {
            case true  => ctlLog(Info , s"Base commit is an ancestor of deploy commit (as it should be).")
            case false => ctlLog(Error, s"Base commit is not an ancestor of deploy commit.") *> exit[Unit](-1)
          }
      }
    )

  def ensureNoRunningDeployments: CtlIO[Unit] =
    gosub(Info, "Ensuring that there is no running deployment.",
      findRunningContainersWithLabel("edu.gemini.commit").flatMap {
        case Nil => ctlLog(Info, "There are no running deployments.")
        case cs  =>
          for {
            c <- config
            _ <- ctlLog(Error, s"There is already a running deployment on ${c.userAndHost.userAndHost}")
            _ <- exit[Unit](-1)
          } yield ()
      }
    )

  def createDatabaseContainer(nDeploy: Int, cDeploy: DeployCommit, iPg: Image): CtlIO[Container] =
    gosub(Info, s"Creating Postgres container from image ${iPg.hash}",
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
        _ <- ctlLog(Info, s"Container is db-${nDeploy} ${c.hash}.")
      } yield c
    )

  def getDeployNumber: CtlIO[Int] =
    for {
      n <- allContainerNames.map { ss =>
             ss.map(_.split("\\-")).collect {
               case Array("db", s)  => s
               case Array("gem", s) => s
               case _               => ""
             } .map(_.parseInt.toOption.getOrElse(0)) .foldLeft(0)(_ max _) + 1
           }
      _ <- ctlLog(Info, s"Deploy number for this host will be $n.")
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
      _ <- if (b) ctlLog(Info, "Created database.")
           else {
             ctlLog(Info, s"Waiting for Postgres to start up.") *>
             shell("sleep 2") *>
             createDatabase(nDeploy, kPg)
           }
    } yield ()

  def awaitHealthy(k: Container): CtlIO[Unit] =
    containerHealth(k) >>= {
      case "starting" => ctlLog(Info,  s"Waiting for health check.") *> shell("sleep 2") *> awaitHealthy(k)
      case "healthy"  => ctlLog(Info,  s"Container is healthy.")
      case s          => ctlLog(Error, s"Health check failed: $s") *> exit(-1)
    }

  def deployDatabase(nDeploy: Int, cDeploy: DeployCommit, iPg: Image): CtlIO[Container] =
    gosub(Info, "Deploying database.",
      for {
        kDb <- createDatabaseContainer(nDeploy, cDeploy, iPg)
        _   <- createDatabase(nDeploy, kDb)
        _   <- awaitHealthy(kDb)
      } yield kDb
    )

  def createGemContainer(nDeploy: Int, cDeploy: DeployCommit, iDeploy: Image) =
    gosub(Info, s"Creating Gem container from image ${iDeploy.hash}",
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
        _  <- ctlLog(Info, s"Container is gem-${nDeploy} ${k.hash}.")
      } yield k
    )

  def awaitNet(host: String, port: Int, retries: Int = 9): CtlIO[Unit] =
    retries match {
      case 0 => ctlLog(Error, "Remote port is unavailable. Hm.") *> exit(-1)
      case n => shell("nc", "-z", host, port.toString).require {
        case Output(0, _) => true
        case Output(1, _) => false
      } flatMap {
        case true  => ctlLog(Info, s"Service is available at $host:$port.")
        case false =>
          ctlLog(Info, s"Awaiting port availability (remaining retries: $n)") *>
          shell("sleep 2") *> awaitNet(host, port, n - 1)
      }
    }

  def deployGem(nDeploy: Int, cDeploy: DeployCommit, iDeploy: Image) =
    gosub(Info, "Deploying Gem.",
      for {
        kGem <- createGemContainer(nDeploy, cDeploy, iDeploy)
        h    <- userAndHost.map(_.host)
        _    <- awaitNet(h, Port)
      } yield kGem
    )

  def deployStandalone(nDeploy: Int, cDeploy: DeployCommit, iDeploy: Image, iPg: Image) =
    gosub(Info, "Performing STANDALONE deployment",
      for {
        _   <- ensureNoRunningDeployments
        kDb <- deployDatabase(nDeploy, cDeploy, iPg)
        _   <- deployGem(nDeploy, cDeploy, iDeploy)
      } yield ()
    )

  def copyData(fromName: String, toContainer: Container): CtlIO[Unit] =
    gosub(Info, s"Copying data from $fromName",
      docker(
        "exec", toContainer.hash,
        "sh", "-c", s"'pg_dump -h $fromName -U postgres gem | psql -q -U postgres -d gem'"
      ) require {
        case Output(0, _) => ()
      }
    )

  def deployUpgrade(nDeploy: Int, cDeploy: DeployCommit, iDeploy: Image, iPg: Image) =
    gosub(Info, "Performing UPGRADE deployment",
      for {
        kGem  <- getRunningGemContainer
        cBase <- getDeployCommitForContainer(kGem)
        _     <- verifyLineage(cBase, cDeploy)
        kPg   <- getRunningPostgresContainer
        cPg   <- getDeployCommitForContainer(kPg)
        _     <- if (cPg.commit === cBase.commit) ctlLog(Info,  "Gem and Postgres containers are at the same commit.")
                 else                             ctlLog(Error, "Gem and Postgres containers are at different commits. What?") *> exit(-1)
        _     <- gosub(Info, "Stopping old Gem container.", stopContainer(kGem))
        kPg2  <- deployDatabase(nDeploy, cDeploy, iPg)
        nPg   <- getContainerName(kPg)
        _     <- copyData(nPg, kPg2)
        _     <- gosub(Info, "Stopping old database container.", stopContainer(kPg))
        _     <- deployGem(nDeploy, cDeploy, iDeploy)
      } yield ()
    )

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
