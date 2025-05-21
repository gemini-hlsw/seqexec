// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import lucuma.core.enums.LightSinkName
import squants.Length

trait InstrumentSpecifics extends InstrumentGuide {
  def calcStepType(config: CleanConfig, isNightSeq: Boolean): Either[SeqexecFailure, StepType] =
    SequenceConfiguration.calcStepType(config, isNightSeq)

  override val oiOffsetGuideThreshold: Option[Length] = None

  // An instrument can request an extra defocus term based on the configuration
  def defocusB(config: CleanConfig): Option[Length] = None

  // The name used for this instrument in the science fold configuration
  def sfName(config: CleanConfig): LightSinkName

}
