// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ctl
package free

import interpreter.Config
import gem.ctl.low.io._

import cats.free.Free, cats.implicits._

/** Module of constructors for the control language. */
object ctl {

  final case class Host(name: String)

  sealed trait Server {
    def userAndHost: String =
      this match {
        case Server.Local               => "localhost"
        case Server.Remote(Host(h), u)  => u.map(_ + "@" + h).getOrElse(h)
      }
  }
  object Server {
    case object Local extends Server
    final case class Remote(host: Host, user: Option[String]) extends Server
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
    final case class  Exit[A](exitCode: Int)                                    extends CtlOp[A]
          case object GetConfig                                                 extends CtlOp[Config]
    final case class  Gosub[A](level: Level, msg: String, fa: CtlIO[A])         extends CtlOp[A]
    final case class  Shell(remote: Boolean, cmd: Either[String, List[String]]) extends CtlOp[Output]
          case object Now                                                       extends CtlOp[Long]
  }

  /** Combinator that applies a check to command output. */
  def require[A](shell: CtlIO[Output])(f: PartialFunction[Output, A]): CtlIO[A] =
    shell.flatMap { o =>
      f.lift(o) match {
        case None    =>
          o.lines.traverse(error(_))      *>
          error(s"exited (${o.exitCode})") *>
          exit[A](o.exitCode)
        case Some(a) => a.pure[CtlIO]
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

  def serverHostName: CtlIO[String] =
    server.map {
      case Server.Local                 => "localhost"
      case Server.Remote(Host(name), _) => name
    }

  def shell(c: String, cs: String*): CtlIO[Output] =
    Free.liftF(CtlOp.Shell(false, Right(c :: cs.toList)))

  def remote(c: String, cs: String*): CtlIO[Output] =
    Free.liftF(CtlOp.Shell(true, Right(c :: cs.toList)))

  def exit[A](exitCode: Int): CtlIO[A] =
    Free.liftF(CtlOp.Exit[A](exitCode))

  val config: CtlIO[Config] =
    Free.liftF(CtlOp.GetConfig)

  val server: CtlIO[Server] =
    config.map(_.server)

  val isRemote: CtlIO[Boolean] =
    server.map {
      case Server.Remote(_, _) => true
      case Server.Local        => false
    }

  def gosub[A](msg: String)(fa: CtlIO[A]): CtlIO[A] =
    Free.liftF(CtlOp.Gosub(Level.Info, msg, fa))

  def logMessage(level: Level, msg: String): CtlIO[Unit] =
    Free.liftF(CtlOp.Gosub(level, msg, ().pure[CtlIO]))

  def now: CtlIO[Long] =
    Free.liftF(CtlOp.Now)

  def info (msg: String): CtlIO[Unit] = logMessage(Level.Info,  msg)
  def warn (msg: String): CtlIO[Unit] = logMessage(Level.Warn,  msg)
  def error(msg: String): CtlIO[Unit] = logMessage(Level.Error, msg)
  def text (msg: String): CtlIO[Unit] = logMessage(Level.Shell, msg)


}
