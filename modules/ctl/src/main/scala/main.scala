// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ctl

import cats.implicits._
import cats.effect._

import gem.ctl.free.ctl._
import gem.ctl.free.interpreter.{ interpreter, InterpreterState }

object main {

  /** Map a `Command` to a corresponding program in `CtlIO`. */
  def command(c: Command): CtlIO[Unit] =
    info(s"Target host is ${c.server.userAndHost}") *> c.impl

  /** Entry point. Parse the commandline args and do what's asked, if possible. */
  def mainʹ(args: List[String]): IO[Unit] =
    for {
      _  <- IO(Console.println) // scalastyle:ignore
      c  <- Command.parse("gemctl", args)
      _  <- c.traverse { c =>
              IORef(InterpreterState.initial)
                .map(interpreter(c, _))
                .flatMap(command(c).foldMap(_).value)
            }
      _  <- IO(Console.println) // scalastyle:ignore
    } yield ()

  def main(args: Array[String]): Unit =
    mainʹ(args.toList).unsafeRunSync

}
