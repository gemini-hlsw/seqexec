// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.server.model

import edu.gemini.seqexec.server.Commands.CommandResult
import edu.gemini.web.common.{CliCommand, RegularCommand, SequenceStatus}

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
