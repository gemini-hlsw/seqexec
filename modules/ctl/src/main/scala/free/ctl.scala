package gem.ctl
package free

import interpreter.Config
import gem.ctl.low.io._

import scalaz._, Scalaz._

/** Module of constructors for the control language. */
object ctl {

  /** Data type for host and optional user. */
  case class UserAndHost(user: Option[String], host: String) {
    def userAndHost: String = user.foldRight(host)(_ + "@" + _)
  }

  /** ADT for output log levels. */
  sealed trait Level
  object Level {
    case object Info  extends Level
    case object Warn  extends Level
    case object Error extends Level
    case object Shell extends Level
  }

  /** ADT of low-level control operations. */
  sealed trait CtlOp[A]
  object CtlOp {
    case class  Exit[A](exitCode: Int)                              extends CtlOp[A]
    case object GetConfig                                           extends CtlOp[Config]
    case class  Gosub[A](level: Level, msg: String, fa: CtlIO[A])   extends CtlOp[A]
    case class  Shell(remote: Boolean, cmd: String \/ List[String]) extends CtlOp[Output]
  }

  /** Combinator that applies a check to command output. */
  def require[A](shell: CtlIO[Output])(f: PartialFunction[Output, A]): CtlIO[A] =
    shell.flatMap { o =>
      f.lift(o) match {
        case None    =>
          o.lines.traverse(error(_))      *>
          error(s"exited (${o.exitCode})") *>
          exit[A](o.exitCode)
        case Some(a) => a.point[CtlIO]
      }
    }

  /** Syntax to allow checks on shell command output. */
  implicit class CtlIOOps[A](fa: CtlIO[A]) {
    def require[B](f: PartialFunction[Output, B])(implicit ev: A =:= Output): CtlIO[B] =
      ctl.require(fa.map(ev))(f)
  }

  /** Free monad over CtlOp. */
  type CtlIO[A] = Free[CtlOp, A]

  // Constructors

  def shell(c: String): CtlIO[Output] =
    Free.liftF(CtlOp.Shell(false, c.left))

  def shell(c: String, cs: String*): CtlIO[Output] =
    Free.liftF(CtlOp.Shell(false, (c :: cs.toList).right))

  def remote(c: String): CtlIO[Output] =
    Free.liftF(CtlOp.Shell(true, c.left))

  def remote(c: String, cs: String*): CtlIO[Output] =
    Free.liftF(CtlOp.Shell(true, (c :: cs.toList).right))

  def exit[A](exitCode: Int): CtlIO[A] =
    Free.liftF(CtlOp.Exit[A](exitCode))

  val config: CtlIO[Config] =
    Free.liftF(CtlOp.GetConfig)

  val userAndHost: CtlIO[UserAndHost] =
    config.map(_.userAndHost)

  def gosub[A](msg: String)(fa: CtlIO[A]): CtlIO[A] =
    Free.liftF(CtlOp.Gosub(Level.Info, msg, fa))

  def logMessage(level: Level, msg: String): CtlIO[Unit] =
    Free.liftF(CtlOp.Gosub(level, msg, ().point[CtlIO]))

  def info (msg: String): CtlIO[Unit] = logMessage(Level.Info,  msg)
  def warn (msg: String): CtlIO[Unit] = logMessage(Level.Warn,  msg)
  def error(msg: String): CtlIO[Unit] = logMessage(Level.Error, msg)
  def text (msg: String): CtlIO[Unit] = logMessage(Level.Shell, msg)


}
