package edu.gemini.seqexec.web.server.model

import edu.gemini.seqexec.server.Commands.CommandResult
import edu.gemini.seqexec.web.common.StepConfig

// Classes exposed to the clients converted to json
case class RegularCommand(command: String, error: Boolean, response: String)
case class SequenceConfig(command: String, error: Boolean, response: String, keys: List[StepConfig])
case class SequenceStatus(command: String, error: Boolean, response: String, steps: List[String])

object CommandsModel {
  def toCommandResult(s: String, r: CommandResult): RegularCommand =
    r.fold(
      l => RegularCommand(s, error = true, l.msg),
      m => RegularCommand(s, error = false, m.msg)
    )

  def toSequenceConfig(s: String, r: CommandResult): SequenceConfig =
    r.fold(
      l => SequenceConfig(s, error = true, l.msg, Nil),
      m => SequenceConfig(s, error = false, m.msg, m.keys.map(Function.tupled(StepConfig.apply)))
    )

  def toSequenceStatus(s: String, r: CommandResult): SequenceStatus =
    r.fold(
      l => SequenceStatus(s, error = true, l.msg, Nil),
      m => SequenceStatus(s, error = false, m.msg, m.steps)
    )
}

