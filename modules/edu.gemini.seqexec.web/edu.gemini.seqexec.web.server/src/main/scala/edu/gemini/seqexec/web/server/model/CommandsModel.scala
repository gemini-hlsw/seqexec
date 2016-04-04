package edu.gemini.seqexec.web.server.model

import edu.gemini.seqexec.server.{CommandError, CommandResponse}
import edu.gemini.seqexec.web.common.StepConfig

import scalaz.\/

// Classes exposed to the clients converted to json
case class CommandResult(command: String, error: Boolean, response: String)
case class SequenceConfig(command: String, error: Boolean, response: String, keys: List[StepConfig])
case class SequenceStatus(command: String, error: Boolean, response: String, steps: List[String])

object CommandsModel {
  def toCommandResult(s: String, r: CommandError \/ CommandResponse): CommandResult = {
    r.fold(
      l => CommandResult(s, error = true, l.msg),
      m => CommandResult(s, error = false, m.msg)
    )
  }

  def toSequenceConfig(s: String, r: CommandError \/ CommandResponse): SequenceConfig = {
    r.fold(
      l => SequenceConfig(s, error = true, l.msg, Nil),
      m => SequenceConfig(s, error = false, m.msg, m.keys.map(Function.tupled(StepConfig.apply)))
    )
  }

  def toSequenceStatus(s: String, r: CommandError \/ CommandResponse): SequenceStatus = {
    r.fold(
      l => SequenceStatus(s, error = true, l.msg, Nil),
      m => SequenceStatus(s, error = false, m.msg, m.steps)
    )
  }
}

