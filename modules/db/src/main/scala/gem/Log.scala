package gem

import gem.dao.LogDao
import doobie.imports._
import java.util.concurrent.{ ExecutorService, Executors, TimeUnit }
import java.util.logging._
import scalaz._, Scalaz._, scalaz.concurrent.Task

/** Logger that logs to the database and a JDK logger, on its own thread. */
case class Log(name: String, xa: Transactor[Task]) { // TODO: construction is an effect

  val jdkLogger: Logger =
    Logger.getLogger(name)

  private val es: ExecutorService =
    Executors.newSingleThreadExecutor()

  private implicit class LogTaskOps(t: Task[_]) {
    def detach[M[_]](implicit delay: Capture[M]): M[Unit] =
      delay {
        Task.fork(t)(es).unsafePerformAsync {
          case -\/(e) => jdkLogger.log(Level.SEVERE, "Database logging failed.", e)
          case \/-(_) => ()
        }
      }
  }

  private def now[M[_]](implicit delay: Capture[M]): M[Long] =
    delay(System.currentTimeMillis)

  private def fail[M[_]: Monad: Catchable: Capture, A](msg: String, elapsed: Long, t: Throwable): M[A] =
    (LogDao.insert(Level.SEVERE, none, msg, Some(t), Some(elapsed)).transact(xa) *>
     Task.delay(jdkLogger.log(Level.SEVERE, s"$msg ($elapsed ms)", t))).detach[M]   *>
    Catchable[M].fail[A](t)

  private def success[M[_]: Monad: Capture, A](msg: String, elapsed: Long, a: A): M[A] =
    (LogDao.insert(Level.INFO, none, msg, None, Some(elapsed)).transact(xa) <*
     Task.delay(jdkLogger.log(Level.INFO, s"$msg ($elapsed ms)"))).detach[M].as(a)

  def instrument[M[_]: Monad: Catchable: Capture, A](ma: M[A], msg: String): M[A] =
    for {
      start   <- now[M]
      disj    <- ma.attempt
      elapsed <- now[M].map(_ - start)
      a       <- disj.fold(t => fail[M, A](msg, elapsed, t), a => success[M, A](msg, elapsed, a))
    } yield a

  def shutdown[M[_]](ms: Long)(implicit delay: Capture[M]): M[Unit] =
    delay {
      es.shutdown
      es.awaitTermination(ms, TimeUnit.MILLISECONDS)
    }

}
