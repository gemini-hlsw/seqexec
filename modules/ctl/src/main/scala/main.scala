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
