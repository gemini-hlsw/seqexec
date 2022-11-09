// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.logging

import java.time.Instant
import cats.effect.IO
import cats.effect.std.Dispatcher
import cats.syntax.all._
import ch.qos.logback.classic.Level
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.AppenderBase
import fs2.concurrent.Topic
import seqexec.model.enum.ServerLogLevel
import seqexec.model.events._

/**
 * Custom appender that can take log events from logback and send them to clients via the common
 * pipe/WebSockets
 *
 * This is out of the scala/http4s loop
 */
class AppenderForClients(out: Topic[IO, SeqexecEvent])(dispatcher: Dispatcher[IO])
    extends AppenderBase[ILoggingEvent] {
  // Remove some loggers. This is a weak form of protection where he don't send some
  // loggers to the cilent, e.g. security related logs
  private val blackListedLoggers = List(""".*\.security\..*""".r)

  override def append(event: ILoggingEvent): Unit = {
    // Convert to a seqexec model to send to clients
    val level     = event.getLevel match {
      case Level.INFO  => ServerLogLevel.INFO.some
      case Level.WARN  => ServerLogLevel.WARN.some
      case Level.ERROR => ServerLogLevel.ERROR.some
      case _           => none
    }
    val timestamp = Instant.ofEpochMilli(event.getTimeStamp)

    // Send a message to the clients if level is INFO or higher
    // We are outside the normal execution loop, thus we need to call unsafePerformSync directly
    dispatcher.unsafeRunAndForget(
      level
        .filter(_ => !blackListedLoggers.exists(_.findFirstIn(event.getLoggerName).isDefined))
        .fold(IO.pure(()))(l => out.publish1(ServerLogMessage(l, timestamp, event.getMessage)).void)
    )
  }
}
