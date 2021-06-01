// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

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
