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
final case class RegularCommand(command: String, error: Boolean, response: String) extends CliCommand
final case class SequenceConfig(command: String, error: Boolean, response: String, keys: StepConfig) extends CliCommand
final case class SequenceStatus(command: String, error: Boolean, response: String, steps: List[String]) extends CliCommand

object CliCommand {
  // Pickler for the commands hierarchy
  @SuppressWarnings(Array("org.wartremover.warts.Throw", "org.wartremover.warts.OptionPartial","org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.ImplicitParameter", "org.wartremover.warts.Equals"))
  implicit val commandsPickler: boopickle.CompositePickler[CliCommand] = compositePickler[CliCommand]
      .addConcreteType[RegularCommand]
      .addConcreteType[SequenceConfig]
      .addConcreteType[SequenceStatus]
}
