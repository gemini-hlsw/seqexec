import scalaz._, Scalaz._
import scalaz.effect._
import scala.util.matching.Regex

import io._
import ctl._
import git._
import docker._
import interp._
import opts._

object main extends SafeApp with Helpers {

  val PrivateNetwork = "gem-net"
  val GemImage       = "gem-telnetd"
  val GemProject     = "telnetd"
  val DeployTagRegex = "^deploy-\\d+$".r

  case class DeployCommit(commit: Commit, uncommitted: Boolean) {
    def imageVersion = if (uncommitted) s"${commit.hash}-UNCOMMITTED" else commit.hash
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
    gosub(Info, "Verifying Gem deploy image.",
      log(Info, s"Image is $GemImage:${dc.imageVersion}.") *>
      findImage(GemImage, dc.imageVersion).flatMap {
        case Some(i) => log(Info, s"Found image ${i.hash}.").as(i)
        case None    =>
          log(Error, s"No such image. Do you need to `sbt $GemProject/docker:publishLocal`?") *>
          exit(-1)
      }
    )

  def fileContentsAtDeployCommit(cDeploy: DeployCommit, path: String): CtlIO[List[String]] =
    if (cDeploy.uncommitted) fileContents(path)
    else fileContentsAtCommit(cDeploy.commit, path)

  def getPostgresImage(cDeploy: DeployCommit): CtlIO[Image] =
    fileContentsAtDeployCommit(cDeploy, "pg-image.txt").flatMap { ss =>
      ss.map(_.trim).filterNot(s => s.startsWith("#") || s.trim.isEmpty) match {
        case List(name) => log(Info, s"Postgres image for this deployment is $name") as Image(name)
        case _          =>
          log(Error, s"Cannot determine Postgres image for deploy commit $cDeploy") *>
          exit(-1)
      }
    }

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
    findContainersWithLabel("edu.gemini.commit").flatMap {
      case Nil  => log(Info, "There are no running deployments.")
      case cs   =>
        log(Error, s"There is already at least one running deployment: ${cs.distinct.map(_.hash).mkString(", ")}") *>
        exit(-1)
    }

  def createDatabaseContainer(nDeploy: Int, cDeploy: DeployCommit, iPg: Image): CtlIO[Container] =
    gosub(Info, s"Creating Postgres container from image ${iPg.hash}",
      for {
        c <- shell("docker", "run",
                  "--detach",
                 s"--net=$PrivateNetwork",
                  "--name", s"db-$nDeploy",
                  "--label", s"edu.gemini.commit=${cDeploy.imageVersion}",
                  "--health-cmd", "psql -U postgres -d gem -c 'select 1'",
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
      _ <- log(Info, s"Deploy number is $n.")
    } yield n

  def createDatabase(nDeploy: Int, kPg: Container): CtlIO[Unit] =
    for {
      b <- shell("docker", "exec", kPg.hash,
             "psql", s"--host=db-$nDeploy",
                      "--command=create database gem",
                      "--username=postgres"
           ).require {
             case Output(0, List("CREATE DATABASE")) => true
             case Output(2, _)                       => false
           }
      _ <- if (b) log(Info, "Created database.")
           else {
             log(Info, s"Waiting for Postgres to start up.") *>
             shell("sleep 1") *>
             createDatabase(nDeploy, kPg)
           }
    } yield ()

  // DEPLOY COMMAND

  def deployDatabase(nDeploy: Int, cDeploy: DeployCommit, iDeploy: Image, iPg: Image) =
    gosub(Info, "Deploying database.",
      for {
        kDb <- createDatabaseContainer(nDeploy, cDeploy, iPg)
        _   <- createDatabase(nDeploy, kDb)
      } yield kDb
    )


  def deployStandalone(nDeploy: Int, cDeploy: DeployCommit, iDeploy: Image, iPg: Image) =
    for {
      _   <- ensureNoRunningDeployments
      kDb <- deployDatabase(nDeploy, cDeploy, iDeploy, iPg)
      _   <- log(Warn, "TODO: standalone deployment")
    } yield ()

  def deployUpgrade(base: Option[String], nDeploy: Int, cDeploy: DeployCommit, iDeploy: Image) =
    for {
      kBase <- getBaseContainer(base, cDeploy)
      _     <- log(Warn, "TODO: upgrade deployment")
    } yield ()

  def deploy(base: Option[String], deploy: String, standalone: Boolean) =
    for {
      nDeploy  <- getDeployNumber
      network  <- getNetwork
      cDeploy  <- getDeployCommit(deploy)
      iPg      <- getPostgresImage(cDeploy)
      iDeploy  <- getDeployImage(cDeploy)
      _        <- if (standalone) deployStandalone(   nDeploy, cDeploy, iDeploy, iPg)
                  else            deployUpgrade(base, nDeploy, cDeploy, iDeploy)
    } yield ()





  val command: Config => CtlIO[Unit] = {
    case Config(d, b, s) => deploy(b, d, s)
  }

  override def runl(args: List[String]): IO[Unit] =
    opts.parse(args) { config =>
      for {
        _ <- IO.putStrLn("")
        i <- IO.newIORef(0)
        _ <- command(config).foldMap(interpreter(i)).run
        _ <- IO.putStrLn("")
      } yield ()
    }

        // find or create gem-net
        // determine deploy commit
        // if upgrade
        //  get base revision (provided, or most recent deploy tag)
        //  find base instance
        //  stop base-gem
        // deploy new-db, new-gem
        // start new-db
        // if upgrade
        //   copy base-db -> new-db
        //   stop base-sb
        // start new-gem
        // await health check

}


trait Helpers {

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

}
