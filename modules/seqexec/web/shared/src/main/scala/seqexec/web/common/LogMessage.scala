// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.common

import java.util.logging.{Level, LogRecord}

import boopickle.Default._

/** Minimal class to transfer log messages from client to server
  * We only care about level and message
  */
final case class LogMessage(level: Level, msg: String)

object LogMessage {
  def fromLogRecord(record: LogRecord): LogMessage = {
    // We may want to encode time, but the timezone on the client cannot be
    // easily recorded. Discard exceptions too
    LogMessage(record.getLevel, record.getMessage)
  }

  // Map levels to numbers to reduce space
  private val Level2IntMapping: Map[Level, Int] = List(Level.OFF, Level.SEVERE,
      Level.WARNING, Level.INFO, Level.CONFIG, Level.FINE, Level.FINER,
      Level.FINEST, Level.ALL).zipWithIndex.toMap

  private val Int2LevelMapping: Map[Int, Level] = Level2IntMapping.map(_.swap)

  implicit val levelPickler: Pickler[Level] =
    transformPickler[java.util.logging.Level, Int](Int2LevelMapping.getOrElse(_, Level.OFF))(Level2IntMapping.getOrElse(_, -1))

}
