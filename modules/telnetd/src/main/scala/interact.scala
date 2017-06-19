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

package gem
package telnetd

import doobie.imports.Transactor
import scalaz._, Scalaz._, scalaz.concurrent.Task
import tuco._, Tuco._

/** Module defining the behavior of our telnet server, parameterized over transactors. */
object Interaction {

  /**
   * Initial state for a command shell session has a logged-in Service value, as well as all the
   * Gem commands and a customized prompt.
   */
  def initialState(service: Service[SessionIO]): Session[Service[SessionIO]] =
    Session.initial(service).copy(
      commands = Builtins[Service[SessionIO]] |+| command.All,
      prompt   = s"${service.user.id}@gem> "
    )

  /**
   * Login loop. Prompt for a password `remainingTries` times and return a Service on success. The
   * transactors are needed for the `Service`.
   */
  def loginLoop(
    user:     String,
    maxTries: Int,
    xa:       Transactor[SessionIO, _],
    txa:      Transactor[Task, _]
  ): SessionIO[Option[Service[SessionIO]]] = {
    def go(remaining: Int): SessionIO[Option[Service[SessionIO]]] =
      if (remaining < 1) {
        Option.empty[Service[SessionIO]].point[SessionIO]
      } else {
        for {
          p <- readLn("Password: ", mask = Some('*'))
          u <- Service.tryLogin(user, p, xa, txa)
          r <- if (u.isEmpty) writeLn("Incorrect password.") *> go(remaining - 1)
               else u.point[SessionIO]
        } yield r
      }
    go(maxTries)
  }

  /**
   * Command shell. Greet the user and run the command shell. Clean up on exit.
   * TODO: per-session log is wasteful and can't be cleaned up reliably
   */
  def runSession(service: Service[SessionIO]): SessionIO[Unit] =
    for {
      d <- SessionIO.delay(new java.util.Date)
      _ <- writeLn(s"Welcome, local time is $d." )
      f <- CommandShell.run(initialState(service))
      _ <- f.data.log.shutdown(1000L)
      _ <- writeLn(s"Goodbye.")
    } yield ()

  /**
   * Entry point for our telnet behavior. If the user logs in successfully the transactors will be
   * associated with the Session.
   */
  def main(xa: Transactor[SessionIO, _], txa: Transactor[Task, _]): SessionIO[Unit] =
    for {
      _ <- writeLn("Welcome to Gem")
      n <- readLn("Username: ")
      s <- loginLoop(n, 3, xa, txa)
      _ <- s.fold(writeLn(s"Login failed."))(runSession)
    } yield ()

}
