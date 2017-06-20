// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ctl
package free

import scalaz._, Scalaz._
import scalaz.effect._

import gem.ctl.low.io._
import gem.ctl.free.ctl._

/** An interpreter for the control language. */
object interpreter {

  /** Configuration for the interpreter. */
  trait Config {
    def verbose: Boolean
    def server:  Server
  }

  final case class InterpreterState(indentation: Int, machineHostCache: Map[Host.Machine, String]) {
    def indent: InterpreterState = copy(indentation = indentation + 1)
    def outdent:InterpreterState = copy(indentation = indentation - 1)
  }
  object InterpreterState {
    val initial: InterpreterState = InterpreterState(0, Map.empty)
  }

  /**
   * Construct an interpreter of `CtlIO` given a `Config` and an `IORef` for the initial state. We
   * carry our state in an `IORef` because it needs to be visible from multiple threads.
   */
  def interpreter(c: Config, state: IORef[InterpreterState]): CtlOp ~> EitherT[IO, Int, ?] = λ[CtlOp ~> EitherT[IO, Int, ?]] {
    case CtlOp.Shell(false, cmd) => doShell(cmd, c.verbose, state)
    case CtlOp.Shell(true,  cmd) =>
      c.server match {

        case Server.Local =>
          doShell(cmd, c.verbose, state)

        case Server.Remote(m @ Host.Machine(h), u) =>
          machineHost(m, c.verbose, state).flatMap { h =>
            doRemoteShell(s"${u.getOrElse("docker")}@$h", cmd, c, state)
          }

        case Server.Remote(Host.Network(h), u) =>
          doRemoteShell(u.foldRight(h)((u, h) => s"$u@$h"), cmd, c, state)

      }
    case CtlOp.Exit(exitCode)    => EitherT.left(exitCode.point[IO])
    case CtlOp.GetConfig         => c.point[EitherT[IO, Int, ?]]
    case CtlOp.Gosub(level, msg, fa) =>
      for {
        _ <- doLog(level, msg, state)
        _ <- EitherT.right(state.mod(_.indent))
        a <- fa.foldMap(this)
        _ <- EitherT.right(state.mod(_.outdent))
      } yield a
  }

  /**
   * Construct a program to log a message to the console at the given log level and indentation.
   * This is where all the colorizing happens.
   */
  @SuppressWarnings(Array("org.wartremover.warts.ToString"))
  private def doLogʹ(level: Level, msg: String, state: IORef[InterpreterState]): IO[Unit] = {
    val color = level match {
      case Level.Error => Console.RED
      case Level.Warn  => Console.YELLOW
      case Level.Info  => Console.GREEN
      case Level.Shell => "\u001B[0;37m" // gray
    }
    val pre = s"[${level.toString.take(4).toLowerCase}]"
    val messageColor = color // if (level == Shell) color else Console.BLUE
    for {
      i <- state.read.map(_.indentation).map("  " * _)
      _ <- IO.putStrLn(f"$color$pre%-7s $messageColor$i$msg${Console.RESET}")
    } yield ()
  }

  /**
   * Construct a program to log a message to the console at the given log level and indentation.
   * Convenience for `doLogʹ` lifted into `EitherT`.
   */
  private def doLog(level: Level, msg: String, state: IORef[InterpreterState]): EitherT[IO, Int, Unit] =
    EitherT.right(doLogʹ(level, msg, state))

  /** Machine name to IP-address. */
  private def machineHost(machine: Host.Machine, verbose: Boolean, state: IORef[InterpreterState]): EitherT[IO, Int, String] =
    EitherT.right(state.read.map(_.machineHostCache.get(machine))).flatMap {
      case Some(s) => s.point[EitherT[IO, Int, ?]]
      case None =>
        doShell(List("docker-machine", "ip", machine.name).right, verbose, state).flatMap {
          case Output(0, s :: Nil) =>
            doLog(Level.Info, s"Address of docker machine '${machine.name}' is $s." , state) *>
            EitherT.right {
              state.mod(is => is.copy(machineHostCache = is.machineHostCache + (machine -> s))).as(s)
            }
          case _ =>
            doLog(Level.Error, "couldn't get ip-address for machine ", state) *>
            EitherT.left(-1.point[IO])
        }
    }

  private def doRemoteShell(uh: String, cmd: String \/ List[String], c: Config, state: IORef[InterpreterState]): EitherT[IO, Int, Output] =
    doShell(cmd.bimap(s => s"ssh $uh $s", "ssh" :: uh :: _), c.verbose, state)

  /**
   * Construct a program to perform a shell operation, optionally logging the output (if verbose),
   * and gathering the result as an `Output`.
   */
  private def doShell(cmd: String \/ List[String], verbose: Boolean, state: IORef[InterpreterState]): EitherT[IO, Int, Output] = {

    def handler(s: String): IO[Unit] =
      if (verbose) doLogʹ(Level.Shell, s, state)
      else IO.putStr(".")

    for {
      _ <- verbose.whenM(doLog(Level.Shell, s"$$ ${cmd.fold(identity, _.mkString(" "))}", state))
      o <- EitherT.right(exec(cmd, handler))
      _ <- verbose.whenM(doLog(Level.Shell, s"exit(${o.exitCode})", state))
      - <- verbose.unlessM(EitherT.right[IO, Int, Unit](IO.putStr("\u001B[1G\u001B[K")))
    } yield o

  }

}
