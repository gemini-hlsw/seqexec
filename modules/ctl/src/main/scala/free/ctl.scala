package gem.ctl
package free

import interpreter.Config
import gem.ctl.low.io._

import scalaz._, Scalaz._

object ctl {

  case class UserAndHost(user: Option[String], host: String) {
    def userAndHost: String = user.foldRight(host)(_ + "@" + _)
  }

  sealed trait Level
  case object Info  extends Level
  case object Warn  extends Level
  case object Error extends Level
  case object Shell extends Level

  sealed trait CtlOp[A]
  case class Shell(remote: Boolean, cmd: String \/ List[String]) extends CtlOp[Output]
  case class Exit[A](exitCode: Int) extends CtlOp[A]
  case class Gosub[A](level: Level, msg: String, fa: CtlIO[A]) extends CtlOp[A]
  case object GetConfig extends CtlOp[Config] // for now

  type CtlIO[A] = Free[CtlOp, A]

  // base constructors
  def shell(c: String):               CtlIO[Output] = Free.liftF(Shell(false, c.left))
  def shell(c: String, cs: String*):  CtlIO[Output] = Free.liftF(Shell(false, (c :: cs.toList).right))
  def remote(c: String):              CtlIO[Output] = Free.liftF(Shell(true, c.left))
  def remote(c: String, cs: String*): CtlIO[Output] = Free.liftF(Shell(true, (c :: cs.toList).right))
  def exit[A](exitCode: Int):         CtlIO[A]      = Free.liftF(Exit[A](exitCode))
  val config:                         CtlIO[Config] = Free.liftF(GetConfig)

  val userAndHost: CtlIO[UserAndHost] = config.map(_.userAndHost)

  def gosub[A](level: Level, msg: String, fa: CtlIO[A]): CtlIO[A] =
    Free.liftF(Gosub(level, msg, fa))

  def log(level: Level, msg: String): CtlIO[Unit]   = gosub(level, msg, ().point[CtlIO])

  def info(msg: String):  CtlIO[Unit] = log(Info, msg)
  def warn(msg: String):  CtlIO[Unit] = log(Warn, msg)
  def error(msg: String): CtlIO[Unit] = log(Error, msg)

  def require[A](shell: CtlIO[Output])(f: PartialFunction[Output, A]): CtlIO[A] =
    shell.flatMap { o =>
      f.lift(o) match {
        case None    =>
          o.lines.traverse(log(Error, _))      *>
          log(Error, s"exited (${o.exitCode})") *>
          exit[A](o.exitCode)
        case Some(a) => a.point[CtlIO]
      }
    }

  implicit class CtlIOOps[A](fa: CtlIO[A]) {
    def require[B](f: PartialFunction[Output, B])(implicit ev: A =:= Output): CtlIO[B] =
      ctl.require(fa.map(ev))(f)
  }

}
