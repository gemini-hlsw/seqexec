import scalaz._, Scalaz._
import scalaz.effect._
import scala.util.matching.Regex

import io._
import ctl._
import git._
import docker._
import interp._

object main extends SafeApp with Helpers {

  val PrivateNetwork = "gem-net"
  val GemImage       = "gem-telnetd"
  val GemProject     = "telnetd"
  val DeployTagRegex = "^deploy-\\d+$".r

  case class DeployCommit(commit: Commit, uncommitted: Boolean) {
    def imageVersion = if (uncommitted) s"${commit.hash}-SNAPSHOT" else commit.hash
  }

  def verifyNetwork: CtlIO[Network] =
    gosub(Info, s"Verifying $PrivateNetwork network.",
      findNetwork(PrivateNetwork).flatMap {
        case Some(n) => log(Info, s"Using existing network ${n.hash}.").as(n)
        case None    => createNetwork(PrivateNetwork) >>! { n => log(Info, s"Created network ${n.hash}.") }
      }
    )

  def verifyBaseCommit(rev: Option[String]): CtlIO[Commit] =
    gosub(Info, "Verifying base commit.",
      commitOr(rev, log(Info, "Finding most recent deploy tag.") *> mostRecentCommitAtTag(DeployTagRegex))
    )

  def verifyDeployCommit(rev: Option[String]): CtlIO[DeployCommit] =
    gosub(Info, "Verifying deploy commit.",
      for {
        c <- commitOr(rev, log(Info, "Using HEAD revision.") *> commitForRevision("HEAD"))
        u <- uncommittedChanges.map(_ && rev.isEmpty)
        _ <- u.whenM(log(Warn, "There are uncommitted changes. This is a SNAPSHOT deployment."))
      } yield DeployCommit(c, u)
    )

  def verifyDeployImage(dc: DeployCommit): CtlIO[Image] =
    gosub(Info, "Verifying deploy image.",
      log(Info, s"Image is $GemImage:${dc.imageVersion}.") *>
      findImage(GemImage, dc.imageVersion).flatMap {
        case Some(i) => log(Info, s"Found image ${i.hash}.").as(i)
        case None    =>
          log(Error, s"No such image. Do you need to `sbt $GemProject/docker:publishLocal`?") *>
          exit(-1)
      }
    )

  def verifyLineage(base: Commit, dc: DeployCommit): CtlIO[Unit] =
    gosub(Info, "Verifying lineage.",
      (dc.uncommitted match {
        case true  => log(Info, "This is a SNAPSHOT deployment, so it's ok if base and deploy are the same.")
        case false => (base === dc.commit).whenM(log(Info, "Base and deploy are the same. Nothing to do!") *> exit(0))
      }) *>
      isAncestor(base, dc.commit).flatMap {
        case true  => log(Info , s"Base commit is an ancestor of deploy commit (as it should be).")
        case false => log(Error, s"Base commit is not an ancestor of deploy commit.") *> exit[Unit](-1)
      }
    )

  def verifyContainer(label: String): CtlIO[Container] =
    findContainersWithLabel(label).flatMap {
      case List(c) => log(Info,  s"Found container ${c.hash}").as(c)
      case Nil     => log(Error, s"No container with label like $label was found.")    *> exit(-1)
      case cs      => log(Error, s"Found multiple containers with label like $label.") *> exit(-1)
    }

  def verifyBaseGemContainer(commit: Commit): CtlIO[Container] =
    gosub(Info, "Verifying base container.",
      verifyContainer(s"edu.gemini.commit=${commit.hash}")
    )




  def deploy(base: Option[String], deploy: Option[String]) =
    for {
      network  <- verifyNetwork
      cBase    <- verifyBaseCommit(base)
      cDeploy  <- verifyDeployCommit(deploy)
      _        <- verifyLineage(cBase, cDeploy)
      iDeploy  <- verifyDeployImage(cDeploy)
      kBase    <- verifyBaseGemContainer(cBase)
    } yield ()

  // gemctl deploy <base> <revision>
  override def runl(args: List[String]): IO[Unit] =
    for {
      _ <- IO.putStrLn("")
      i <- IO.newIORef(0)
      _ <- deploy(args.lift(0), args.lift(1)).foldMap(interpreter(i)).run
      _ <- IO.putStrLn("")
    } yield ()



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
