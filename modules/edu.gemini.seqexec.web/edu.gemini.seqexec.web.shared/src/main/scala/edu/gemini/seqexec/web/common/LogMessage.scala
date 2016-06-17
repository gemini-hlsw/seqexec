package edu.gemini.seqexec.web.common

import java.util.logging.{Level, LogRecord}

import upickle.Js

import scala.collection.breakOut

/** Minimal class to transfer log messages from client to server
  * We only care about level and message
  */
case class LogMessage(level: Level, msg: String)

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

  // Pickler for Level components
  implicit val levelWriter = upickle.default.Writer[Level]{
    case i => Js.Num(Level2IntMapping.getOrElse(i, -1).toDouble)
  }

  implicit val levelReader = upickle.default.Reader[Level]{
    // default to level.OFF though it should not happen as the maps are complete
    case Js.Num(i) => Int2LevelMapping.getOrElse(i.toInt, Level.OFF)
  }
}