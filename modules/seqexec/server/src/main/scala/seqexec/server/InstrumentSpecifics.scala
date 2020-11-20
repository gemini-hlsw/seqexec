package seqexec.server

import seqexec.model.enum.Instrument

trait InstrumentSpecifics {
  val instrument: Instrument

  def calcStepType(config: CleanConfig, isNightSeq: Boolean): Either[SeqexecFailure, StepType] =
    SequenceConfiguration.calcStepType(config, isNightSeq)

}
