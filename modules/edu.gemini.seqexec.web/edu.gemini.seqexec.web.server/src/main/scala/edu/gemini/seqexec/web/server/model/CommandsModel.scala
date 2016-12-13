package edu.gemini.seqexec.web.server.model

import edu.gemini.seqexec.server.Commands.CommandResult
import edu.gemini.seqexec.web.common._

object CommandsModel {
  def toCommandResult(s: String, r: CommandResult): CliCommand =
    r.fold(
      l => RegularCommand(s, error = true, l.msg),
      m => RegularCommand(s, error = false, m.msg)
    )

  /*def toSequenceConfig(s: String, r: CommandResult): CliCommand =
    r.fold(
      l => SequenceConfig(s, error = true, l.msg, Nil),
      m => SequenceConfig(s, error = false, m.msg, m.keys.map(Function.tupled(StepConfig.apply)))
    )*/

  def toSequenceStatus(s: String, r: CommandResult): CliCommand =
    r.fold(
      l => SequenceStatus(s, error = true, l.msg, Nil),
      m => SequenceStatus(s, error = false, m.msg, m.steps)
    )
}
