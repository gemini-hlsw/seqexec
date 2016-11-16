package gem
package telnetd

import gem.dao.UserDao
import gem.enum.ProgramRole

import doobie.imports.{ Capture => DCapture, Transactor }
import scalaz._, Scalaz._, scalaz.effect._, scalaz.concurrent.Task
import tuco._, Tuco._

/** Module defining the behavior of our telnet server, parameterized over transactors. */
final case class Interaction(xa: Transactor[SessionIO], txa: Transactor[Task]) {

  /**
   * Initial state for a command shell session has a logged-in Service value, as well as all the
   * Gem commands and a customized prompt.
   */
  def initialState(service: Service[SessionIO]): Session[Service[SessionIO]] =
    Session.initial(service).copy(
      commands = Builtins[Service[SessionIO]] |+| command.All,
      prompt   = s"${service.user.id}@gem> "
    )

  /** Login loop. Prompt for a password `remainingTries` times and return a Service on success. */
  def loginLoop(user: String, remainingTries: Int): SessionIO[Option[Service[SessionIO]]] =
    if (remainingTries < 1) {
      Option.empty[Service[SessionIO]].point[SessionIO]
    } else {
      for {
        p <- readLn("Password: ", mask = Some('*'))
        u <- Service.tryLogin(user, p, xa, txa)
        r <- if (u.isEmpty) writeLn("Incorrect password.") *> loginLoop(user, remainingTries - 1)
             else u.point[SessionIO]
      } yield r
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

  /** Entry point for our telnet behavior. */
  val main: SessionIO[Unit] =
    for {
      _ <- writeLn("Welcome to Gem")
      n <- readLn("Username: ")
      s <- loginLoop(n, 3)
      _ <- s.fold(writeLn(s"Login failed."))(runSession)
    } yield ()

}
