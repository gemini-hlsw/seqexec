package gem
package telnetd

import gem.dao.UserDao
import gem.enum.ProgramRole

import doobie.imports.{ Capture => DCapture, DriverManagerTransactor }
import tuco.imports._
import scalaz._, Scalaz._, scalaz.effect._

object Interaction {

  implicit val ConectionIODoobieCapture: DCapture[ConnectionIO] =
    new DCapture[ConnectionIO] {
      def apply[A](a: => A) = FC.delay(a)
    }

  val xa  = DriverManagerTransactor[ConnectionIO]("org.postgresql.Driver","jdbc:postgresql:gem","postgres","")
  val txa = DriverManagerTransactor[scalaz.concurrent.Task]("org.postgresql.Driver","jdbc:postgresql:gem","postgres","")

  def initialState(data: Data): Session[Data] =
    Session.initial(data).copy(
      commands = Builtins[Data] |+| GemCommands.all,
      prompt   = s"${data.service.user.id}@gem> "
    )

  def tryLogin(user: String, pass: String): ConnectionIO[Option[Service[ConnectionIO]]] =
    xa.transact(UserDao.selectWithRoles(user, pass)).flatMap {
      case None    => Option.empty[Service[ConnectionIO]].point[ConnectionIO]
      case Some(u) => Log.newLog[ConnectionIO]("teletd:user", txa).map(l => Some(Service[ConnectionIO](xa, l, u)))
    }

  def loginLoop(user: String, remainingTries: Int): ConnectionIO[Option[Service[ConnectionIO]]] =
    if (remainingTries < 1) Option.empty[Service[ConnectionIO]].point[ConnectionIO]
    else {
      for {
        p <- HC.readLn("Password: ")
        u <- tryLogin(user, p)
        r <- if (u.isEmpty) HC.writeLn("Incorrect user and/or password.") *> loginLoop(user, remainingTries - 1)
             else u.point[ConnectionIO]
      } yield r
    }

  def interact: ConnectionIO[Unit] =
    for {
      _ <- HC.writeLn("Welcome to Gem.")
      n <- HC.readLn("Username: ")
      s <- loginLoop(n, 3)
      _ <- s.fold(HC.writeLn(s"Login failed.")) {
        s =>
          val data = Data(s)
          for {
            _ <- HC.writeLn(s"Hello.")
            f <- CommandShell.run(initialState(data))
            _ <- HC.writeLn(s"Goodbye.")
          } yield ()
      }
    } yield ()

}
