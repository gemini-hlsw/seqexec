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
  private val Int2LevelMapping: Map[Int, Level] = Map(0 -> Level.OFF,
    1 -> Level.SEVERE,
    3 -> Level.WARNING,
    4 -> Level.INFO,
    5 -> Level.CONFIG,
    6 -> Level.FINE,
    7 -> Level.FINER,
    8 -> Level.FINEST,
    9 -> Level.ALL)

  private val Level2IntMapping: Map[Level, Int] = Int2LevelMapping.map {
    case (i, l) => l -> i
  }(breakOut)

  // Pickler for Level components
  implicit val levelWriter = upickle.default.Writer[Level]{
    case i => Js.Num(Level2IntMapping.getOrElse(i, -1).toDouble)
  }

  implicit val levelReader = upickle.default.Reader[Level]{
    // default to level.OFF though it should not happen as the maps are complete
    case Js.Num(i) => Int2LevelMapping.getOrElse(i.toInt, Level.OFF)
  }
}