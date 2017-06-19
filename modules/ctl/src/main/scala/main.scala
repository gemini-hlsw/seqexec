// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ctl

import scalaz._, Scalaz._
import scalaz.effect._

import gem.ctl.free.ctl._
import gem.ctl.free.interpreter.{ interpreter, InterpreterState }

import gem.ctl.hi.ps.ps
import gem.ctl.hi.log.showLog
import gem.ctl.hi.stop.stop
import gem.ctl.hi.deploy.deploy
import gem.ctl.hi.rollback.rollback

object main extends SafeApp {

  /** Map a `Command` to a corresponding program in `CtlIO`. */
  def command(c: Command): CtlIO[Unit] =
    info(s"Target host is ${c.server.userAndHost}").as(c).flatMap {
      case Command.Deploy(u, d, s, v, f) => deploy(d, s, f)
      case Command.Ps(_, _)              => ps
      case Command.Stop(_, _)            => stop
      case Command.Log(_, _, n)          => showLog(n)
      case Command.Rollback(_, _)        => rollback
    }

  /** Entry point. Parse the commandline args and do what's asked, if possible. */
  override def runl(args: List[String]): IO[Unit] =
    for {
      _  <- IO.putStrLn("")
      c  <- Command.parse("gemctl", args)
      _  <- c.traverse { c =>
              IO.newIORef(InterpreterState.initial)
                .map(interpreter(c, _))
                .flatMap(command(c).foldMap(_).run)
            }
      _  <- IO.putStrLn("")
    } yield ()

}
