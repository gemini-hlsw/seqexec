package seqexec.server

import lucuma.core.enum.LightSinkName
import squants.Length

trait InstrumentSpecifics extends InstrumentGuide {
  def calcStepType(config: CleanConfig, isNightSeq: Boolean): Either[SeqexecFailure, StepType] =
    SequenceConfiguration.calcStepType(config, isNightSeq)

  override val oiOffsetGuideThreshold: Option[Length] = None

  // The name used for this instrument in the science fold configuration
  def sfName(config: CleanConfig): LightSinkName

}
