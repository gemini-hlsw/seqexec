// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import gem.util.Enumerated

sealed trait AlignAndCalibStep extends Product with Serializable

object AlignAndCalibStep {
  case object NoAction extends AlignAndCalibStep
  case object StartGuiding extends AlignAndCalibStep
  case object StopGuiding extends AlignAndCalibStep
  case object SuperContOff extends AlignAndCalibStep
  case object OMSSEntShutterOff extends AlignAndCalibStep
  case object CalExistShutterOff extends AlignAndCalibStep
  case object ArtSourceDeploy extends AlignAndCalibStep
  case object AoDarks extends AlignAndCalibStep
  case object SuperContOn extends AlignAndCalibStep
  case object CalFlags extends AlignAndCalibStep
  case object Twt2Lens extends AlignAndCalibStep
  case object CalExitShutterOn extends AlignAndCalibStep
  case object ArtSourceExtract extends AlignAndCalibStep
  case object OMSSEntShutterOn extends AlignAndCalibStep
  case object InputFoldTracking extends AlignAndCalibStep
  case object Done extends AlignAndCalibStep

  /** @group Typeclass Instances */
  implicit val AlignAndCalibStepEnumerated: Enumerated[AlignAndCalibStep] =
    Enumerated.of(NoAction, StartGuiding, StopGuiding, SuperContOff, OMSSEntShutterOff, CalExistShutterOff, ArtSourceDeploy, AoDarks, SuperContOn, CalFlags, Twt2Lens, CalExitShutterOn, ArtSourceExtract, OMSSEntShutterOn, InputFoldTracking, Done)

  def fromInt(i: Int): AlignAndCalibStep = i match {
    case 0  => StartGuiding
    case 1  => StopGuiding
    case 2  => SuperContOff
    case 3  => OMSSEntShutterOff
    case 4  => CalExistShutterOff
    case 5  => ArtSourceDeploy
    case 6  => AoDarks
    case 7  => SuperContOn
    case 8  => CalFlags
    case 9  => Twt2Lens
    case 10 => CalExitShutterOn
    case 11 => ArtSourceExtract
    case 12 => OMSSEntShutterOn
    case 13 => InputFoldTracking
    case 14 => Done
    case _  => NoAction
  }
}
