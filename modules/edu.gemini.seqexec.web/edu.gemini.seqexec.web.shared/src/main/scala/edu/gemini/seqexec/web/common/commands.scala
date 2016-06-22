package edu.gemini.seqexec.web.common

import boopickle.Default._

sealed trait CliCommand {
  def command: String
  def error: Boolean
  def response: String
}

// Classes exposed to web clients, uses only scala classes
case class RegularCommand(command: String, error: Boolean, response: String) extends CliCommand
case class SequenceConfig(command: String, error: Boolean, response: String, keys: List[StepConfig]) extends CliCommand
case class SequenceStatus(command: String, error: Boolean, response: String, steps: List[String]) extends CliCommand

object CliCommand {
  // Pickler for the commands hierarchy
  implicit val commandsPickler = compositePickler[CliCommand]
      .addConcreteType[RegularCommand]
      .addConcreteType[SequenceConfig]
      .addConcreteType[SequenceStatus]
}