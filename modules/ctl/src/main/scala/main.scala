import scalaz._, Scalaz._
import scalaz.effect._
import scala.util.matching.Regex

import io._
import ctl._
import git._
import docker._
import interp._
import opts.{ Command, DeployOpts, PsOpts, StopOpts }

object main extends SafeApp with DeployImpl with PsImpl with StopImpl {

  def command(c: Command): CtlIO[Unit] =
    log(Info, s"Target host is ${c.userAndHost.userAndHost}").as(c).flatMap {
      case DeployOpts(u, d, b, s, v) => deploy(b, d, s)
      case PsOpts(_, _)              => ps
      case StopOpts(_, _)            => stop
    }

  override def runl(args: List[String]): IO[Unit] =
    for {
      _ <- IO.putStrLn("")
      c <- opts.parse("gemctl", args)
      _ <- c.traverse { c =>
          IO.newIORef(0)
            .map(interpreter(c, _))
            .flatMap(command(c).foldMap(_).run)
      }
      _ <- IO.putStrLn("")
    } yield ()

}


trait PsImpl {

  val ps: CtlIO[Unit] =
    for {
      ks <- findContainersWithLabel("edu.gemini.commit")
      _  <- ks.traverseU(psOne)
    } yield ()

  def psOne(k: Container): CtlIO[Unit] =
    docker.docker(
      "inspect",
      "--format", "'{{ index .Config.Labels \"edu.gemini.commit\"}},{{.Name}},{{.State.Status}}'",
      k.hash
    ) require {
      case Output(0, s :: Nil) => s.split(",").toList
    } flatMap {
      case List(c, n, s) => log(Info, s"${k.hash}  $c  $s  $n")
      case ss            => log(Error, s"Bogus response: ${ss.mkString(",")}") *> exit(-1)
    }

}

trait StopImpl {

  val stop: CtlIO[Unit] =
    gosub(Info, "Shutting down Gem deployment.",
      for {
        ks <- findContainersWithLabel("edu.gemini.commit")
        _  <- ks.traverseU(stopOne)
      } yield ()
    )

  def stopOne(k: Container): CtlIO[Unit] =
    log(Info, s"Stopping $k") *>
    docker.docker(
      "stop",
      k.hash
    ) require {
      case Output(0, s :: Nil) => ()
    }

}


trait DeployImpl {

  val PrivateNetwork = "gem-net"
  val GemOrg         = "geminihlsw"
  val GemImage       = "gem-telnetd"
  val GemProject     = "telnetd"
  val DeployTagRegex = "^deploy-\\d+$".r
  val Port           = 1234

  case class DeployCommit(commit: Commit, uncommitted: Boolean) {
    def imageVersion = if (uncommitted) s"${commit.hash}-UNCOMMITTED" else commit.hash
  }

  /** Get the commit for the given revision, or compute one with `fa`. */
  def commitOr(rev: Option[String], fa: CtlIO[Commit]): CtlIO[Commit] =
    rev.fold(gosub(Info,  "No revision specified.", fa))(s =>
             log(Info, s"Requested revision is $s.") *> commitForRevision(s)
    ) >>! (c => log(Info, s"Commit is ${c.hash}."))

  /** Return the most recent deploy commit, or fail. */
  def mostRecentCommitAtTag(re: Regex): CtlIO[Commit] =
    mostRecentTag(re).flatMap {
      case None    => log(Error, s"No tags matching pattern $re were found.") *> exit[Commit](-1)
      case Some(t) => log(Info,  s"Found ${t.tag}.")                    *> commitForRevision(t.tag)
    }

  def getNetwork: CtlIO[Network] =
    gosub(Info, s"Verifying $PrivateNetwork network.",
      findNetwork(PrivateNetwork).flatMap {
        case Some(n) => log(Info, s"Using existing network ${n.hash}.").as(n)
        case None    => createNetwork(PrivateNetwork) >>! { n => log(Info, s"Created network ${n.hash}.") }
      }
    )

  def getBaseCommit(rev: Option[String]): CtlIO[Commit] =
    gosub(Info, "Verifying base commit.",
      commitOr(rev, log(Info, "Finding most recent deploy tag.") *> mostRecentCommitAtTag(DeployTagRegex))
    )

  def getDeployCommit(rev: String): CtlIO[DeployCommit] =
    gosub(Info, "Verifying deploy commit.",
      for {
        c <- log(Info, s"Using $rev.") *> commitForRevision(rev)
        _ <- log(Info, s"Commit is ${c.hash}")
        u <- uncommittedChanges.map(_ && rev == "HEAD")
        _ <- u.whenM(log(Warn, "There are uncommitted changes. This is a UNCOMMITTED deployment."))
      } yield DeployCommit(c, u)
    )

  def getDeployImage(dc: DeployCommit): CtlIO[Image] =
    gosub(Info, "Verifying Gem deploy image.", requireImage(s"$GemOrg/$GemImage:${dc.imageVersion}"))

  def fileContentsAtDeployCommit(cDeploy: DeployCommit, path: String): CtlIO[List[String]] =
    if (cDeploy.uncommitted) fileContents(path)
    else fileContentsAtCommit(cDeploy.commit, path)

  def requireImage(nameAndVersion: String): CtlIO[Image] = {
    log(Info, s"Looking for $nameAndVersion") *>
    findImage(nameAndVersion).flatMap {
      case None    =>
        log(Warn, s"Cannot find image locally. Pulling (could take a few minutes).") *> pullImage(nameAndVersion).flatMap {
          case None    => log(Error, s"Image was not found.") *> exit(-1)
          case Some(i) => log(Info, s"Image is ${i.hash}") as i
        }
      case Some(i) => log(Info, s"Image is ${i.hash}") as i
    }
  }

  def getPostgresImage(cDeploy: DeployCommit): CtlIO[Image] =
    gosub(Info, "Verifying Postgres deploy image.",
      fileContentsAtDeployCommit(cDeploy, "pg-image.txt").flatMap { ss =>
        ss.map(_.trim).filterNot(s => s.startsWith("#") || s.trim.isEmpty) match {
          case List(name) => requireImage(name)
          case _          =>
            log(Error, s"Cannot determine Postgres image for deploy commit $cDeploy") *>
            exit(-1)
        }
      }
    )

  def getLineage(base: Commit, dc: DeployCommit): CtlIO[Unit] =
    gosub(Info, "Verifying lineage.",
      (dc.uncommitted match {
        case true  => log(Info, "This is a UNCOMMITTED deployment, so it's ok if base and deploy are the same.")
        case false => (base === dc.commit).whenM(log(Info, "Base and deploy are the same. Nothing to do!") *> exit(0))
      }) *>
      isAncestor(base, dc.commit).flatMap {
        case true  => log(Info , s"Base commit is an ancestor of deploy commit (as it should be).")
        case false => log(Error, s"Base commit is not an ancestor of deploy commit.") *> exit[Unit](-1)
      }
    )

  def getContainer(label: String): CtlIO[Container] =
    findContainersWithLabel(label).flatMap {
      case List(c) => log(Info,  s"Found container ${c.hash}").as(c)
      case Nil     => log(Error, s"No container with label like $label was found.")    *> exit(-1)
      case cs      => log(Error, s"Found multiple containers with label like $label.") *> exit(-1)
    }

  def getBaseGemContainer(commit: Commit): CtlIO[Container] =
    gosub(Info, "Verifying base container.",
      getContainer(s"edu.gemini.commit=${commit.hash}")
    )

  def getBaseContainer(base: Option[String], cDeploy: DeployCommit): CtlIO[Container] =
    for {
      cBase    <- getBaseCommit(base)
      _        <- getLineage(cBase, cDeploy)
      kBase    <- getBaseGemContainer(cBase)
    } yield kBase

  def ensureNoRunningDeployments: CtlIO[Unit] =
    gosub(Info, "Ensuring that there is no running deployment.",
      findContainersWithLabel("edu.gemini.commit").flatMap {
        case Nil => log(Info, "There are no running deployments.")
        case cs  =>
          for {
            c <- config
            _ <- log(Error, s"There is already a running deployment on ${c.userAndHost.userAndHost}")
            _ <- exit[Unit](-1)
          } yield ()
      }
    )

  def createDatabaseContainer(nDeploy: Int, cDeploy: DeployCommit, iPg: Image): CtlIO[Container] =
    gosub(Info, s"Creating Postgres container from image ${iPg.hash}",
      for {
        c <- docker.docker("run",
                  "--detach",
                 s"--net=$PrivateNetwork",
                  "--name", s"db-$nDeploy",
                  "--label", s"edu.gemini.commit=${cDeploy.imageVersion}",
                  "--health-cmd", "\"psql -U postgres -d gem -c 'select 1'\"",
                  "--health-interval", "10s",
                  "--health-retries", "2",
                  "--health-timeout", "2s",
                  iPg.hash
             ).require { case Output(0, List(s)) => Container(s) }
        _ <- log(Info, s"Container is db-${nDeploy} ${c.hash}.")
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
      _ <- log(Info, s"Deploy number for this host will be $n.")
    } yield n

  def createDatabase(nDeploy: Int, kPg: Container): CtlIO[Unit] =
    for {
      b <- docker.docker("exec", kPg.hash,
             "psql", s"--host=db-$nDeploy",
                      "--command='create database gem'",
                      "--username=postgres"
           ).require {
             case Output(0, List("CREATE DATABASE")) => true
             case Output(2, _)                       => false
           }
      _ <- if (b) log(Info, "Created database.")
           else {
             log(Info, s"Waiting for Postgres to start up.") *>
             shell("sleep 2") *>
             createDatabase(nDeploy, kPg)
           }
    } yield ()

  def awaitHealthy(k: Container): CtlIO[Unit] =
    containerHealth(k) >>= {
      case "starting" => log(Info,  s"Waiting for health check.") *> shell("sleep 2") *> awaitHealthy(k)
      case "healthy"  => log(Info,  s"Container is healthy.")
      case s          => log(Error, s"Health check failed: $s") *> exit(-1)
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
        k  <- docker.docker("run",
                "--detach",
                "--tty",
                "--interactive",
               s"--net=$PrivateNetwork",
                "--name",    s"gem-$nDeploy",
                "--label",   s"edu.gemini.commit=${cDeploy.imageVersion}",
                "--publish", s"$Port:6666",
                "--env",     s"GEM_DB_URL=jdbc:postgresql://db-$nDeploy/gem",
                iDeploy.hash
              ).require { case Output(0, List(s)) => Container(s) }
        _  <- log(Info, s"Container is gem-${nDeploy} ${k.hash}.")
      } yield k
    )

  def awaitNet(host: String, port: Int, retries: Int = 9): CtlIO[Unit] =
    retries match {
      case 0 => log(Error, "Remote port is unavailable. Hm.") *> exit(-1)
      case n => shell("nc", "-z", host, port.toString).require {
        case Output(0, _) => true
        case Output(1, _) => false
      } flatMap {
        case true  => log(Info, s"Service is available at $host:$port.")
        case false =>
          log(Info, s"Awaiting port availability (remaining retries: $n)") *>
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
    for {
      _   <- ensureNoRunningDeployments
      kDb <- deployDatabase(nDeploy, cDeploy, iPg)
      _   <- deployGem(nDeploy, cDeploy, iDeploy)
    } yield ()

  def deployUpgrade(base: Option[String], nDeploy: Int, cDeploy: DeployCommit, iDeploy: Image) =
    for {
      kBase <- getBaseContainer(base, cDeploy)
      _     <- log(Warn, "TODO: upgrade deployment")
    } yield ()

  def deploy(base: Option[String], deploy: String, standalone: Boolean) =
    for {
      nDeploy  <- getDeployNumber
      cDeploy  <- getDeployCommit(deploy)
      iPg      <- getPostgresImage(cDeploy)
      iDeploy  <- getDeployImage(cDeploy)
      network  <- getNetwork
      _        <- if (standalone) deployStandalone(   nDeploy, cDeploy, iDeploy, iPg)
                  else            deployUpgrade(base, nDeploy, cDeploy, iDeploy)
    } yield ()

}
