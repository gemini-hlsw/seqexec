package edu.gemini.seqexec.web.server.logging

import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.AppenderBase

/**
 * Custom appender that can take log events from logback and send them
 * to clients via the common pipe/WebSockets
 */
class AppenderForClients extends AppenderBase[ILoggingEvent] {
  override def append(event: ILoggingEvent): Unit = {
    println("Event " + event)
  }
}
