package gem

import gem.dao.LogDao
import doobie.imports._
import java.util.logging._
import scalaz._, Scalaz._

case class Log[M[_]: Monad: Capture: Catchable](name: String, xa: Transactor[M]) {

  val jdkLogger: Logger =
    Logger.getLogger(name)

  private val now: M[Long] =
    Capture[M].apply(System.currentTimeMillis)

  private def fail[A](msg: String, elapsed: Long)(t: Throwable): M[A] =
    LogDao.insert(Level.SEVERE, none, msg, Some(t), Some(elapsed)).transact(xa) *> // todo, what if logging fails?
    Capture[M].apply(jdkLogger.log(Level.SEVERE, s"$msg ($elapsed ms)", t)) *>
    Catchable[M].fail[A](t)

  private def success[A](msg: String, elapsed: Long)(a: A): M[A] =
    LogDao.insert(Level.INFO, none, msg, None, Some(elapsed)).transact(xa).as(a) <*
    Capture[M].apply(jdkLogger.log(Level.INFO, s"$msg ($elapsed ms)"))

  def instrument[A](ma: M[A], msg: String): M[A] =
    for {
      start   <- now
      disj    <- ma.attempt
      elapsed <- now.map(_ - start)
      a       <- disj.fold(fail(msg, elapsed), success(msg, elapsed))
    } yield a

}
