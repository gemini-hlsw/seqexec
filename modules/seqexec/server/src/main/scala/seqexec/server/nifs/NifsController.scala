// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.nifs

import cats.Show
import seqexec.model.dhs.ImageFileId
import seqexec.server.nifs.NifsController.DCConfig
import seqexec.server.nifs.NifsController.NifsConfig
import seqexec.server.ObserveCommand
import seqexec.server.Progress
import squants.Time
import shapeless.tag.@@

trait NifsController[F[_]] {

  def applyConfig(config: NifsConfig): F[Unit]

  def observe(fileId: ImageFileId, cfg: DCConfig): F[ObserveCommand.Result]

  def endObserve: F[Unit]

  def stopObserve: F[Unit]

  def abortObserve: F[Unit]

  def observeProgress(total: Time): fs2.Stream[F, Progress]

  def calcTotalExposureTime(cfg: DCConfig): F[Time]

}

trait CentralWavelengthD
trait MaskOffsetD
trait CoaddsI
trait PeriodI
trait NumberOfFowSamplesI
trait NumberOfSamplesI
trait NumberOfResetsI
trait NumberOfPeriodsI

sealed trait WindowCover extends Product with Serializable

object WindowCover {
  case object Closed extends WindowCover
  case object Opened extends WindowCover
}

object NifsController {
  // DC
  type Coadds          = Int @@ CoaddsI
  type Period          = Int @@ PeriodI
  type ExposureTime    = Time
  type NumberOfResets  = Int @@ NumberOfResetsI
  type NumberOfPeriods = Int @@ NumberOfPeriodsI
  type NumberOfSamples = Int @@ NumberOfSamplesI
  // this one is calculated out of the ReadMode
  type NumberOfFowSamples = Int @@ NumberOfFowSamplesI
  type ReadMode           = edu.gemini.spModel.gemini.nifs.NIFSParams.ReadMode
  type EngReadMode        = edu.gemini.spModel.gemini.nifs.NIFSParams.EngReadMode

  // CC
  type Filter            = edu.gemini.spModel.gemini.nifs.NIFSParams.Filter
  type Mask              = edu.gemini.spModel.gemini.nifs.NIFSParams.Mask
  type Disperser         = edu.gemini.spModel.gemini.nifs.NIFSParams.Disperser
  type ImagingMirror     = edu.gemini.spModel.gemini.nifs.NIFSParams.ImagingMirror
  type CentralWavelength = Double @@ CentralWavelengthD
  type MaskOffset        = Double @@ MaskOffsetD

  final case class DCConfig(coadds:          Coadds,
                            period:          Option[Period],
                            exposureTime:    ExposureTime,
                            numberOfResets:  Option[NumberOfResets],
                            numberOfPeriods: Option[NumberOfPeriods],
                            numberOfSamples: Option[NumberOfSamples],
                            readMode:        Either[EngReadMode, ReadMode])

  sealed trait CCConfig extends Product with Serializable

  case object DarkCCConfig extends CCConfig
  final case class StdCCConfig(filter:        Filter,
                               mask:          Mask,
                               disperser:     Disperser,
                               imagingMirror: ImagingMirror,
                               wavelength:    CentralWavelength,
                               maskOffset:    MaskOffset,
                               windowCover:   WindowCover)
      extends CCConfig

  final case class NifsConfig(cc: CCConfig, dc: DCConfig)

  implicit val cfgShow: Show[NifsConfig] = Show.fromToString

}
