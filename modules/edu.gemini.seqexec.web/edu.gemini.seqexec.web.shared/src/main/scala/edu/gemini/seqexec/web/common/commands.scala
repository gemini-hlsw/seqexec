// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.common

import boopickle.Default._
import edu.gemini.seqexec.model.Model.StepConfig

sealed trait CliCommand {
  def command: String
  def error: Boolean
  def response: String
}

// Classes exposed to web clients, uses only scala classes
case class RegularCommand(command: String, error: Boolean, response: String) extends CliCommand
case class SequenceConfig(command: String, error: Boolean, response: String, keys: StepConfig) extends CliCommand
case class SequenceStatus(command: String, error: Boolean, response: String, steps: List[String]) extends CliCommand

object CliCommand {
  // Pickler for the commands hierarchy
  implicit val commandsPickler = compositePickler[CliCommand]
      .addConcreteType[RegularCommand]
      .addConcreteType[SequenceConfig]
      .addConcreteType[SequenceStatus]
}
