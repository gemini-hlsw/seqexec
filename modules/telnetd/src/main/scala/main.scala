package gem
package telnetd

import tuco._, Tuco._
import doobie.imports.DriverManagerTransactor
import scalaz.effect._, scalaz.concurrent.Task

/**
 * Entry point for running Gem with a telnet server. This will go away at some point and the telnet
 * server will be one of several services.
 */
object Main extends SafeApp {

  // We need two transactors, one for things we do inside our telnet session and another for logging.
  val xa  = DriverManagerTransactor[SessionIO]("org.postgresql.Driver","jdbc:postgresql:gem","postgres","")
  val txa = DriverManagerTransactor[Task     ]("org.postgresql.Driver","jdbc:postgresql:gem","postgres","")

  override def runc: IO[Unit] =
    Config(Interaction.main(xa, txa), 6666).run(simpleServer)

}
