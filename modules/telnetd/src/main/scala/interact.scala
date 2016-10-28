package gem
package telnetd

import gem.dao.UserDao
import gem.enum.ProgramRole

import doobie.imports.{ Capture => DCapture, DriverManagerTransactor }
import scalaz._, Scalaz._, scalaz.effect._
import tuco._, Tuco._

object Interaction {

  val xa  = DriverManagerTransactor[SessionIO]("org.postgresql.Driver","jdbc:postgresql:gem","postgres","")
  val txa = DriverManagerTransactor[scalaz.concurrent.Task]("org.postgresql.Driver","jdbc:postgresql:gem","postgres","")

  def initialState(service: Service[SessionIO]): Session[Service[SessionIO]] =
    Session.initial(service).copy(
      commands = Builtins[Service[SessionIO]] |+| command.All,
      prompt   = s"${service.user.id}@gem> "
    )

  def tryLogin(user: String, pass: String): SessionIO[Option[Service[SessionIO]]] =
    xa.transact(UserDao.selectWithRoles(user, pass)).flatMap {
      case None    => Option.empty[Service[SessionIO]].point[SessionIO]
      case Some(u) => Log.newLog[SessionIO]("teletd:user", txa).map(l => Some(Service[SessionIO](xa, l, u)))
    }

  def loginLoop(user: String, remainingTries: Int): SessionIO[Option[Service[SessionIO]]] =
    if (remainingTries < 1) {
      Option.empty[Service[SessionIO]].point[SessionIO]
    } else {
      for {
        p <- readLn("Password: ", mask = Some('*'))
        u <- tryLogin(user, p)
        r <- if (u.isEmpty) writeLn("Incorrect password.") *> loginLoop(user, remainingTries - 1)
             else u.point[SessionIO]
      } yield r
    }

  def runSession(service: Service[SessionIO]): SessionIO[Unit] =
    for {
      d <- SessionIO.delay(new java.util.Date)
      _ <- writeLn(s"Welcome, local time is $d." )
      f <- CommandShell.run(initialState(service))
      _ <- f.data.log.shutdown(1000L) // TODO: per-session log is wasteful and unreliable
      _ <- writeLn(s"Goodbye.")
    } yield ()

  def interact: SessionIO[Unit] =
    for {
      _ <- writeLn("Welcome to Gem")
      n <- readLn("Username: ")
      s <- loginLoop(n, 3)
      _ <- s.fold(writeLn(s"Login failed."))(runSession)
    } yield ()

}
