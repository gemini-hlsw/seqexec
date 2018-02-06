// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server.gnirs

import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.seqexec.server.gnirs.GnirsController.GnirsConfig
import edu.gemini.seqexec.server.{ObserveCommand, SeqAction}
import squants.Time

import scalaz.Show
//scalastyle:off
trait GnirsController {

  def applyConfig(config: GnirsConfig): SeqAction[Unit]

  def observe(fileId: ImageFileId, expTime: Time): SeqAction[ObserveCommand.Result]

  // endObserve is to notify the completion of the observation, not to cause its end.
  def endObserve: SeqAction[Unit]

  def stopObserve: SeqAction[Unit]

  def abortObserve: SeqAction[Unit]

}

object GnirsController {

  sealed trait Mode

  case object Acquisition extends Mode

  sealed abstract class Spectrography(val disperser: Disperser) extends Mode

  final case class CrossDisperserS(override val disperser: Disperser) extends Spectrography(disperser)

  final case class CrossDisperserL(override val disperser: Disperser) extends Spectrography(disperser)

  final case class Wollaston(override val disperser: Disperser) extends Spectrography(disperser)

  final case class Mirror(override val disperser: Disperser) extends Spectrography(disperser)

  type Camera = edu.gemini.spModel.gemini.gnirs.GNIRSParams.Camera

  type Coadds = Int

  type Decker = edu.gemini.spModel.gemini.gnirs.GNIRSParams.Decker

  type Disperser = edu.gemini.spModel.gemini.gnirs.GNIRSParams.Disperser

  type ExposureTime = Time

  sealed trait Filter1
  object Filter1 {
    object Open extends Filter1
    object ND100X extends Filter1
    object Y_MK extends Filter1
    object J_MK extends Filter1
    object K_MK extends Filter1
    object PupilViewer extends Filter1
  }

  sealed trait Filter2
  object Filter2 {
    object Open extends Filter2
    object H extends Filter2
    object J extends Filter2
    object K extends Filter2
    object L extends Filter2
    object M extends Filter2
    object X extends Filter2
    object XD extends Filter2
    object H2 extends Filter2
    object PAH extends Filter2
  }

  type ReadMode = edu.gemini.spModel.gemini.gnirs.GNIRSParams.ReadMode

  sealed trait SlitWidth
  object SlitWidth {
    case object Slit0_10 extends SlitWidth
    case object Slit0_15 extends SlitWidth
    case object Slit0_20 extends SlitWidth
    case object Slit0_30 extends SlitWidth
    case object Slit0_45 extends SlitWidth
    case object Slit0_68 extends SlitWidth
    case object Slit1_00 extends SlitWidth
    case object PupilViewer extends SlitWidth
    case object SmallPinhole extends SlitWidth
    case object LargePinhole extends SlitWidth
    case object Acquisition extends SlitWidth
  }

  type WellDepth = edu.gemini.spModel.gemini.gnirs.GNIRSParams.WellDepth

  type WollanstonPrism = edu.gemini.spModel.gemini.gnirs.GNIRSParams.WollastonPrism

  final case class DCConfig(exposureTime: ExposureTime,
                            coadds: Coadds,
                            readMode: ReadMode,
                            wellDepth: WellDepth
                           )

  sealed trait CCConfig

  case object Dark extends CCConfig

  final case class Other(mode: Mode,
                         camera: Camera,
                         decker: Decker,
                         filter1: Filter1,
                         filter2: Filter2,
                         slitWidth: Option[SlitWidth]
                        ) extends CCConfig

  final case class GnirsConfig(cc: CCConfig, dc: DCConfig)

  implicit val cfgShow: Show[GnirsConfig] = Show.showA[GnirsConfig]

}
