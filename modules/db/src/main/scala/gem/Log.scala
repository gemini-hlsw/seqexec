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
