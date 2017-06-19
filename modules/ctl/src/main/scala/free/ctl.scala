/*
 * Copyright (c) 2017, Association of Universities for Research in Astronomy, Inc. (AURA)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package gem.ctl
package free

import interpreter.Config
import gem.ctl.low.io._

import scalaz._, Scalaz._

/** Module of constructors for the control language. */
object ctl {

  sealed trait Host {
    def name: String
  }
  object Host {
    case class Machine(name: String) extends Host
    case class Network(name: String) extends Host
  }

  sealed trait Server {
    def userAndHost: String =
      this match {
        case Server.Local                       => "localhost"
        case Server.Remote(Host.Machine(h), u)  => u.foldRight(h)(_ + "@" + _) + " (docker machine)"
        case Server.Remote(Host.Network(h), u)  => u.foldRight(h)(_ + "@" + _)
      }
  }
  object Server {
    case object Local extends Server
    case class  Remote(host: Host, user: Option[String]) extends Server
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

  def serverHostName: CtlIO[String] =
    server.flatMap {
      case Server.Local => "localhost".point[CtlIO]
      case Server.Remote(Host.Network(name), _) => name.point[CtlIO]
      case Server.Remote(Host.Machine(name), _) =>
        shell("docker-machine", "ip", name).require {
          case Output(0, s :: Nil) => s
        }
    }

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
    Free.liftF(CtlOp.Gosub(level, msg, ().point[CtlIO]))

  def info (msg: String): CtlIO[Unit] = logMessage(Level.Info,  msg)
  def warn (msg: String): CtlIO[Unit] = logMessage(Level.Warn,  msg)
  def error(msg: String): CtlIO[Unit] = logMessage(Level.Error, msg)
  def text (msg: String): CtlIO[Unit] = logMessage(Level.Shell, msg)


}
