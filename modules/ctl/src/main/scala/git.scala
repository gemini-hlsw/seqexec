import scalaz._, Scalaz._
import scala.util.matching.Regex

import ctl._
import io._

object git {

  case class Commit(hash: String)
  object Commit {
    implicit val OrderCommit: Order[Commit] = Order.orderBy(_.hash)
  }

  case class Tag(tag: String)

  // Yields most recent tag matching the given pattern, if any
  def mostRecentTag(pattern: Regex): CtlIO[Option[Tag]] =
    shell("git", "tag").require {
      case Output(0, ts) => ts.find(s => pattern.findFirstIn(s).isDefined).map(Tag)
    }

  // Yields the commit associated with the given revision, or fails with a nice message
  def commitForRevision(rev: String): CtlIO[Commit] =
    shell("git", "rev-parse", rev).require {
      case Output(0, List(s)) => Some(Commit(s))
      case Output(128, _)     => None
    } .flatMap {
      case Some(c) => c.point[CtlIO]
      case None    => log(Error, s"No such revsion: $rev") *> exit[Commit](128)
    }

  val uncommittedChanges: CtlIO[Boolean] =
    shell("git status -s").require {
      case Output(0, deltas) => deltas.nonEmpty
    }

  // is c1 an ancestor of c2?
  def isAncestor(c1: Commit, c2: Commit): CtlIO[Boolean] =
    shell("git", "merge-base", "--is-ancestor", c1.hash, c2.hash).require {
      case Output(0, Nil) => true
      case Output(1, Nil) => false
    }

  def fileContentsAtCommit(c: Commit, path: String): CtlIO[List[String]] =
    shell("git", "show", s"${c.hash}:$path").require {
      case Output(0, ss) => ss
    }

  def fileContents(path: String): CtlIO[List[String]] =
    shell("cat", path).require {
      case Output(0, ss) => ss
    }

}
