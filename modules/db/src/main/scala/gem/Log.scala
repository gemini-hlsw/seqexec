// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.implicits._, cats.effect._
import doobie._, doobie.implicits._
import gem.dao.LogDao
import java.util.concurrent.{ ExecutorService, Executors, TimeUnit }
import java.util.logging._

/** Logger that logs to the database and a JDK logger, on its own thread, in effect type M. */
abstract class Log[M[_]] private (name: String, xa: Transactor[IO]) {
  implicit val M: Sync[M]

  val jdkLogger: Logger =
    Logger.getLogger(name)

  private val es: ExecutorService =
    Executors.newSingleThreadExecutor()

  private implicit class LogTaskOps(t: IO[_]) {
    def detach: M[Unit] =
      M.delay {
        es.execute { () =>
          t.unsafeRunAsync {
            case Left(e) => jdkLogger.log(Level.SEVERE, "Database logging failed.", e)
            case Right(_) => ()
          }
        }
      }
  }

  private def now: M[Long] =
    M.delay(System.currentTimeMillis)

  private def fail[A](user: User[_], msg: => String, elapsed: Long, t: Throwable): M[A] =
    (LogDao.insert(user, Level.SEVERE, none, msg, Some(t), Some(elapsed)).transact(xa) *>
     IO(jdkLogger.log(Level.SEVERE, s"${user.id}: $msg ($elapsed ms)", t))).detach   *>
    M.raiseError[A](t)

  private def success[A](user: User[_], msg: => String, elapsed: Long, a: A): M[A] =
    (LogDao.insert(user, Level.INFO, none, msg, None, Some(elapsed)).transact(xa) *>
     IO(jdkLogger.log(Level.INFO, s"${user.id}: $msg ($elapsed ms)"))).detach.as(a)

  /**
   * Construct a new program in `M` that is equivalent to `ma` but also logs a message, elapsed
   * time and (in case of failure) a stacktrace to the database and to a JDK logger. Logging is
   * asynchronous so messages may not appear immediately.
   */
  def log[A](user: User[_], msg: => String)(ma: M[A]): M[A] =
    for {
      start   <- now
      disj    <- ma.attempt
      elapsed <- now.map(_ - start)
      a       <- disj.fold(t => fail(user, msg, elapsed, t), a => success(user, msg, elapsed, a))
    } yield a

  /**
   * Constructs a new program in `M` that simply writes a message without an
   * associated timed action.  Roughly equivalent to `log(user, msg)(().pure[M])`
   * but guaranteed to record 0 elapsed time.
   */
  def logMessage(user: User[_], msg: => String): M[Unit] =
    success(user, msg, 0, ())

  def shutdown(ms: Long): M[Unit] =
    M.delay {
      jdkLogger.info("Log shutdown requested.")
      es.shutdown
      es.awaitTermination(ms, TimeUnit.MILLISECONDS)
      jdkLogger.info("Log shutdown complete.")
    }

}

object Log {

  /** Program in M to construct a Log, also in M. */
  def newLog[M[_]: Sync](name: String, xa: Transactor[IO]): M[Log[M]] =
    newLogIn[M, M](name, xa)

  /** Program in F to construct a Log in M. */
  def newLogIn[M[_]: Sync, F[_]: Sync](name: String, xa: Transactor[IO]): F[Log[M]] =
    Sync[F].delay(new Log[M](name, xa) {
      val M: Sync[M] = Sync[M]
    })

}
