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

import tuco._, Tuco._
import doobie.imports._
import org.flywaydb.core.Flyway
import scalaz._, scalaz.effect._, scalaz.concurrent.Task

/**
 * Entry point for running Gem with a telnet server. This will go away at some point and the telnet
 * server will be one of several services.
 */
object Main extends SafeApp {

  /** When we start the app with docker we pass arguments as environment variables. */
  val ENV_GEM_DB_URL  = "GEM_DB_URL"
  val ENV_GEM_DB_USER = "GEM_DB_USER"
  val ENV_GEM_DB_PASS = "GEM_DB_PASS"

  /** Get an environment variable. */
  def getEnv(key: String, default: String): IO[String] =
    IO(sys.env.getOrElse(key, default))

  /** Construct a transactor with the give effect type. */
  def xa[M[_]: Monad: Capture: Catchable](url: String, user: String, pass: String): Transactor[M, _] =
    DriverManagerTransactor[M]("org.postgresql.Driver", url, user, pass)

  /** Run migrations. */
  def migrate(url: String, user: String, pass: String): IO[Int] =
    IO {
      val flyway = new Flyway()
      flyway.setDataSource(url, user, pass);
      flyway.migrate()
    }

  override def runc: IO[Unit] =
    for {
      url  <- getEnv(ENV_GEM_DB_URL,  "jdbc:postgresql:gem")
      user <- getEnv(ENV_GEM_DB_USER, "postgres")
      pass <- getEnv(ENV_GEM_DB_URL,  "")
      _    <- IO.putStrLn(s"Connecting with URL $url, user $user, pass «hidden»")
      _    <- migrate(url, user, pass)
      sxa  = xa[SessionIO](url, user, pass)
      txa  = xa[Task     ](url, user, pass)
      _    <- Config(Interaction.main(sxa, txa), 6666).run(simpleServer)
    } yield ()

}
