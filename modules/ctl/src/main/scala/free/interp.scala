// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ctl
package free

import cats._, cats.data._, cats.implicits._
import cats.effect._

import gem.ctl.low.io._
import gem.ctl.free.ctl._

/** An interpreter for the control language. */
object interpreter {

  /** Configuration for the interpreter. */
  final case class Config(
    verbose: Boolean,
    server:  Server,
    ansi:    Boolean
  )

  final case class InterpreterState(indentation: Int) {
    def indent:  InterpreterState = copy(indentation = indentation + 1)
    def outdent: InterpreterState = copy(indentation = indentation - 1)
  }
  object InterpreterState {
    val initial: InterpreterState = InterpreterState(0)
  }

  /**
   * Construct an interpreter of `CtlIO` given a `Config` and an `IORef` for the initial state. We
   * carry our state in an `IORef` because it needs to be visible from multiple threads.
   */
  def interpreter(c: Config, state: IORef[InterpreterState]): CtlOp ~> EitherT[IO, Int, ?] =
    λ[CtlOp ~> EitherT[IO, Int, ?]] {
      case CtlOp.Shell(false, cmd) => doShell(cmd, c.verbose, c.ansi, state)
      case CtlOp.Shell(true,  cmd) =>
        c.server match {

          case Server.Local =>
            doShell(cmd, c.verbose, c.ansi, state)

          case Server.Remote(Host(h), u) =>
            doRemoteShell(u.map(u =>  s"$u@$h").getOrElse(h), cmd, c, state)

        }
      case CtlOp.Exit(exitCode)    => EitherT.left(exitCode.pure[IO])
      case CtlOp.GetConfig         => c.pure[EitherT[IO, Int, ?]]
      case CtlOp.Gosub(level, msg, fa) =>
        for {
          _ <- doLog(c.ansi, level, msg, state)
          _ <- EitherT.right(state.mod(_.indent))
          a <- fa.foldMap(this)
          _ <- EitherT.right(state.mod(_.outdent))
        } yield a
      case CtlOp.Now => EitherT.right(IO(System.currentTimeMillis))
    }

  /**
   * Construct a program to log a message to the console at the given log level and indentation.
   * This is where all the colorizing happens.
   */
  @SuppressWarnings(Array("org.wartremover.warts.ToString"))
  private def doLogʹ(ansi: Boolean, level: Level, msg: String, state: IORef[InterpreterState]): IO[Unit] = {
    lazy val color =
      if (ansi) {
        level match {
          case Level.Error => Console.RED
          case Level.Warn  => Console.YELLOW
          case Level.Info  => Console.GREEN
          case Level.Shell => "\u001B[0;37m" // gray
        }
      } else ""
    val pre  = s"$color[${level.toString.take(4).toLowerCase}] $color"
    val post = if (ansi) Console.RESET else ""
    for {
      i <- state.read.map(_.indentation).map("  " * _)
      _ <- IO(Console.println(f"$pre$i$msg$post")) // scalastyle:ignore
    } yield ()
  }

  /**
   * Construct a program to log a message to the console at the given log level and indentation.
   * Convenience for `doLogʹ` lifted into `EitherT`.
   */
  private def doLog(ansi: Boolean, level: Level, msg: String, state: IORef[InterpreterState]): EitherT[IO, Int, Unit] =
    EitherT.right(doLogʹ(ansi, level, msg, state))

  private def doRemoteShell(uh: String, cmd: Either[String, List[String]], c: Config, state: IORef[InterpreterState]): EitherT[IO, Int, Output] =
    doShell(cmd.bimap(s => s"ssh $uh $s", "ssh" :: uh :: _), c.verbose, c.ansi, state)

  /**
   * Construct a program to perform a shell operation, optionally logging the output (if verbose),
   * and gathering the result as an `Output`.
   */
  private def doShell(cmd: Either[String, List[String]], verbose: Boolean, ansi: Boolean, state: IORef[InterpreterState]): EitherT[IO, Int, Output] = {

    def handler(s: String): IO[Unit] =
      if (verbose) doLogʹ(ansi, Level.Shell, s, state)
      else IO.unit

    for {
      _ <- doLog(ansi, Level.Shell, s"$$ ${cmd.fold(identity, _.mkString(" "))}", state).whenA(verbose)
      o <- EitherT.right(exec(cmd, handler))
      _ <-doLog(ansi, Level.Shell, s"exit(${o.exitCode})", state).whenA(verbose)
    } yield o

  }

}
