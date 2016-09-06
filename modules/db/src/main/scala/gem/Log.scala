package gem

import gem.dao.LogDao
import doobie.imports._
import java.util.concurrent.{ ExecutorService, Executors, TimeUnit }
import java.util.logging._
import scalaz._, Scalaz._, scalaz.concurrent.Task

/** Logger that logs to the database and a JDK logger, on its own thread, in effect type M. */
class Log[M[_]: Monad: Catchable] private (name: String, xa: Transactor[Task], delay: Capture[M]) {

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

  private def fail[A](msg: String, elapsed: Long, t: Throwable): M[A] =
    (LogDao.insert(Level.SEVERE, none, msg, Some(t), Some(elapsed)).transact(xa) *>
     Task.delay(jdkLogger.log(Level.SEVERE, s"$msg ($elapsed ms)", t))).detach   *>
    Catchable[M].fail[A](t)

  private def success[A](msg: String, elapsed: Long, a: A): M[A] =
    (LogDao.insert(Level.INFO, none, msg, None, Some(elapsed)).transact(xa) *>
     Task.delay(jdkLogger.log(Level.INFO, s"$msg ($elapsed ms)"))).detach.as(a)

  /**
   * Construct a new program in `M` that is equivalent to `ma` but also logs a message, elapsed
   * time and (in case of failure) a stacktrace to the database and to a JDK logger. Logging is
   * asynchronous so messages may not appear immediately.
   */
  def instrument[A](ma: M[A], msg: String): M[A] =
    for {
      start   <- now
      disj    <- ma.attempt
      elapsed <- now.map(_ - start)
      a       <- disj.fold(t => fail(msg, elapsed, t), a => success(msg, elapsed, a))
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

  def newLog[M[_]: Monad: Catchable](name: String, xa: Transactor[Task])(implicit delay: Capture[M]): M[Log[M]] =
    delay(new Log(name, xa, delay))

}
