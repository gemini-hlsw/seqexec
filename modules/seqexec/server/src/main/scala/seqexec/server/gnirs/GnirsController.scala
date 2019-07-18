// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gnirs

import cats.Applicative
import cats.Eq
import cats.Show
import cats.implicits._
import edu.gemini.spModel.gemini.gnirs.GNIRSParams.{ ReadMode => LegacyReadMode }
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.ObserveCommandResult
import seqexec.server.gnirs.GnirsController.GnirsConfig
import seqexec.server.Progress
import squants.{Length, Time}
import squants.time.TimeConversions._

trait GnirsController[F[_]] {

  def applyConfig(config: GnirsConfig): F[Unit]

  def observe(fileId: ImageFileId, expTime: Time): F[ObserveCommandResult]

  // endObserve is to notify the completion of the observation, not to cause its end.
  def endObserve: F[Unit]

  def stopObserve: F[Unit]

  def abortObserve: F[Unit]

  def observeProgress(total: Time): fs2.Stream[F, Progress]

  def calcTotalExposureTime(cfg: GnirsController.DCConfig): F[Time]
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

  type Wavelength = Length

  sealed trait Filter1
  object Filter1 {
    case object Open extends Filter1
    case object ND100X extends Filter1
    case object Y_MK extends Filter1
    case object J_MK extends Filter1
    case object K_MK extends Filter1
    case object PupilViewer extends Filter1
    case object RightMask extends Filter1
    case object LeftMask extends Filter1
  }

  sealed trait Filter2Pos
  object Filter2Pos {
    case object Open extends Filter2Pos
    case object H extends Filter2Pos
    case object J extends Filter2Pos
    case object K extends Filter2Pos
    case object L extends Filter2Pos
    case object M extends Filter2Pos
    case object X extends Filter2Pos
    case object XD extends Filter2Pos
    case object H2 extends Filter2Pos
    case object PAH extends Filter2Pos
  }

  sealed trait Filter2
  case object Auto extends Filter2
  final case class Manual(f: Filter2Pos) extends Filter2

  sealed trait Focus
  object Focus {
    case object Best extends Focus
    final case class Manual(v: Int) extends Focus
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

    implicit val eq: Eq[SlitWidth] = Eq.fromUniversalEquals
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
                         focus: Focus,
                         wavel: Wavelength,
                         slitWidth: Option[SlitWidth]
                        ) extends CCConfig

  final case class GnirsConfig(cc: CCConfig, dc: DCConfig)

  implicit val cfgShow: Show[GnirsConfig] = Show.fromToString

  def calcTotalExposureTime[F[_]: Applicative](cfg: GnirsController.DCConfig): F[Time] = {
    val readOutTime = cfg.readMode match {
      case LegacyReadMode.VERY_BRIGHT => 0.19
      case LegacyReadMode.BRIGHT => 0.69
      case LegacyReadMode.FAINT => 11.14
      case LegacyReadMode.VERY_FAINT => 22.31
    }

    (cfg.coadds * (cfg.exposureTime + readOutTime.seconds)).pure[F]
  }
}
