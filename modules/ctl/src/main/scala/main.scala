// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ctl

import cats.implicits._
import cats.effect._

import gem.ctl.free.interpreter.{ interpreter, InterpreterState }

object main {

  /** Entry point. Parse the commandline args and do what's asked, if possible. */
  def mainʹ(args: List[String]): IO[Int] =
    for {
      _  <- IO(Console.println) // scalastyle:ignore
      c  <- Parsers.parse("gemctl", args)
      n  <- c.traverse { case (config, impl) =>
              IORef(InterpreterState.initial)
                .map(interpreter(config, _))
                .flatMap(impl.foldMap(_).value)
            }
      _  <- IO(Console.println) // scalastyle:ignore
    } yield n.fold(0)(_.fold(identity, _ => 0))

  def main(args: Array[String]): Unit = 
    sys.exit(mainʹ(args.toList).unsafeRunSync)

}
