package gem

import gem.dao.LogDao
import doobie.imports._
import java.util.concurrent.{ ExecutorService, Executors, TimeUnit }
import java.util.logging._
import scalaz._, Scalaz._, scalaz.concurrent.Task

/** Logger that logs to the database and a JDK logger, on its own thread, in effect type M. */
class Log[M[_]: Monad: Catchable] private (name: String, xa: Transactor[Task, _], delay: Capture[M]) {

  val jdkLogger: Logger =
    Logger.getLogger(name)

  private val es: ExecutorService =
    Executors.newSingleThreadExecutor()

  private implicit class LogTaskOps(t: Task[_]) {
    def detach: M[Unit] =
      delay {
        Task.fork(t)(es).unsafePerformAsync {
          case -\/(e) => jdkLogger.log(Level.SEVERE, "Database logging failed.", e)
          case \/-(_) => ()
        }
      }
  }

  private def now: M[Long] =
    delay(System.currentTimeMillis)

  private def fail[A](user: User[_], msg: => String, elapsed: Long, t: Throwable): M[A] =
    (LogDao.insert(user, Level.SEVERE, none, msg, Some(t), Some(elapsed)).transact(xa) *>
     Task.delay(jdkLogger.log(Level.SEVERE, s"${user.id}: $msg ($elapsed ms)", t))).detach   *>
    Catchable[M].fail[A](t)

  private def success[A](user: User[_], msg: => String, elapsed: Long, a: A): M[A] =
    (LogDao.insert(user, Level.INFO, none, msg, None, Some(elapsed)).transact(xa) *>
     Task.delay(jdkLogger.log(Level.INFO, s"${user.id}: $msg ($elapsed ms)"))).detach.as(a)

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

  def shutdown(ms: Long): M[Unit] =
    delay {
      jdkLogger.info("Log shutdown requested.")
      es.shutdown
      es.awaitTermination(ms, TimeUnit.MILLISECONDS)
      jdkLogger.info("Log shutdown complete.")
    }

}

object Log {

  def newLog[M[_]: Monad: Catchable](name: String, xa: Transactor[Task, _])(implicit delay: Capture[M]): M[Log[M]] =
    delay(new Log(name, xa, delay))

}
