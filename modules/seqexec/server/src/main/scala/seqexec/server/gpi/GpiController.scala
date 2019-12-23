// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gpi

import cats._
import cats.effect.Sync
import cats.implicits._
import edu.gemini.spModel.gemini.gpi.Gpi.{ Apodizer => LegacyApodizer }
import edu.gemini.spModel.gemini.gpi.Gpi.{ Adc => LegacyAdc }
import edu.gemini.spModel.gemini.gpi.Gpi.{ ArtificialSource => LegacyArtificialSource }
import edu.gemini.spModel.gemini.gpi.Gpi.{ Disperser => LegacyDisperser }
import edu.gemini.spModel.gemini.gpi.Gpi.{ FPM => LegacyFPM }
import edu.gemini.spModel.gemini.gpi.Gpi.{ Filter => LegacyFilter }
import edu.gemini.spModel.gemini.gpi.Gpi.{ Lyot => LegacyLyot }
import edu.gemini.spModel.gemini.gpi.Gpi.{ ObservingMode => LegacyObservingMode }
import edu.gemini.spModel.gemini.gpi.Gpi.{ PupilCamera => LegacyPupilCamera }
import edu.gemini.spModel.gemini.gpi.Gpi.{ Shutter => LegacyShutter }
import edu.gemini.aspen.giapi.commands.HandlerResponse.Response
import gem.enum.GiapiStatusApply._
import gem.enum.GpiReadMode
import giapi.client.commands.Configuration
import giapi.client.commands.CommandResultException
import giapi.client.gpi.GpiClient
import giapi.client.GiapiStatusDb
import io.chrisdavenport.log4cats.Logger
import mouse.boolean._
import scala.concurrent.duration._
import seqexec.server.keywords.GdsClient
import seqexec.server.SeqexecFailure
import seqexec.server.GiapiInstrumentController
import seqexec.server.AbstractGiapiInstrumentController

final case class AOFlags(useAo:      Boolean,
                         useCal:     Boolean,
                         aoOptimize: Boolean,
                         alignFpm:   Boolean,
                         magH:       Double,
                         magI:       Double)

object AOFlags {
  implicit val eq: Eq[AOFlags]     = Eq.fromUniversalEquals
  implicit val show: Show[AOFlags] = Show.fromToString
}

final case class ArtificialSources(ir:          LegacyArtificialSource,
                                   vis:         LegacyArtificialSource,
                                   sc:          LegacyArtificialSource,
                                   attenuation: Double)

object ArtificialSources extends GpiConfigEq {
  implicit val eq: Eq[ArtificialSources] =
    Eq.by(x => (x.ir, x.vis, x.sc, x.attenuation))
  implicit val show: Show[ArtificialSources] = Show.fromToString
}

final case class Shutters(entranceShutter:     LegacyShutter,
                          calEntranceShutter:  LegacyShutter,
                          calScienceShutter:   LegacyShutter,
                          calReferenceShutter: LegacyShutter)

object Shutters extends GpiConfigEq {
  implicit val eq: Eq[Shutters] = Eq.by(
    x =>
      (x.entranceShutter,
       x.calEntranceShutter,
       x.calScienceShutter,
       x.calReferenceShutter))
  implicit val show: Show[Shutters] = Show.fromToString
}

final case class NonStandardModeParams(apodizer: LegacyApodizer,
                                       fpm:      LegacyFPM,
                                       lyot:     LegacyLyot,
                                       filter:   LegacyFilter)

object NonStandardModeParams extends GpiConfigEq {
  implicit val eq: Eq[NonStandardModeParams] =
    Eq.by(x => (x.apodizer, x.fpm, x.lyot, x.filter))
  implicit val show: Show[NonStandardModeParams] = Show.fromToString
}

sealed abstract class ReadoutArea(val startX: Int,
                                  val startY: Int,
                                  val endX: Int,
                                  val endY: Int)

object ReadoutArea {
  val MinValue: Int = 0
  val MaxValue: Int = 2047

  private def inRange(x: Int): Boolean = x >= MinValue && x <= MaxValue

  val DefaultArea: ReadoutArea =
    new ReadoutArea(MinValue, MinValue, MaxValue, MaxValue) {}

  def fromValues(startX: Int, startY: Int, endX: Int, endY: Int): Option[ReadoutArea] =
    if (inRange(startX) && inRange(startY) && inRange(endX) && inRange(endY)) {
      (new ReadoutArea(startX, startY, endX, endY) {}).some
    } else {
      none
    }

  implicit val eqRa: Eq[ReadoutArea] = Eq.by(x =>
    (x.startX, x.startY, x.endX, x.endY))
}


sealed trait GpiConfig extends Product with Serializable

final case class RegularGpiConfig(
  adc:            LegacyAdc,
  expTime:        Duration,
  coAdds:         Int,
  readMode:       GpiReadMode,
  area:           ReadoutArea,
  mode:           Either[LegacyObservingMode, NonStandardModeParams],
  disperser:      LegacyDisperser,
  disperserAngle: Double,
  shutters:       Shutters,
  asu:            ArtificialSources,
  pc:             LegacyPupilCamera,
  aoFlags:        AOFlags) extends GpiConfig

object RegularGpiConfig extends GpiConfigEq {
  implicit val eq: Eq[RegularGpiConfig] = Eq.by(
    x =>
      (x.adc,
       x.expTime,
       x.coAdds,
       x.readMode,
       x.area,
       x.mode,
       x.disperser,
       x.disperserAngle,
       x.shutters,
       x.asu,
       x.pc,
       x.aoFlags))
}

case object AlignAndCalibConfig extends GpiConfig {
  val config: Configuration =
    Configuration.single(GpiAlignAndCalib.applyItem, GpiClient.ALIGN_AND_CALIB_DEFAULT_MODE)

  override def toString: String = s"$config"

}

trait GpiController[F[_]] extends GiapiInstrumentController[F, GpiConfig] {
  def gdsClient: GdsClient[F]

  def statusDb: GiapiStatusDb[F]

  def alignAndCalib: F[Unit]
}

trait GpiConfigEq {
  implicit val apodizerEq: Eq[LegacyApodizer] = Eq.by(_.displayValue)

  implicit val adcEq: Eq[LegacyAdc] = Eq.by(_.displayValue)

  implicit val omEq: Eq[LegacyObservingMode] = Eq.by(_.displayValue)

  implicit val dispEq: Eq[LegacyDisperser] = Eq.by(_.displayValue)

  implicit val fpmEq: Eq[LegacyFPM] = Eq.by(_.displayValue)

  implicit val filterEq: Eq[LegacyFilter] = Eq.by(_.displayValue)

  implicit val lyotEq: Eq[LegacyLyot] = Eq.by(_.displayValue)

  implicit val shEq: Eq[LegacyShutter] = Eq.by(_.displayValue)

  implicit val asEq: Eq[LegacyArtificialSource] = Eq.by(_.displayValue)

  implicit val pcEq: Eq[LegacyPupilCamera] = Eq.by(_.displayValue)
}

object GpiController extends GpiLookupTables with GpiConfigEq {

  private def obsModeConfiguration(config: RegularGpiConfig): Configuration =
    config.mode.fold(
      m =>
        Configuration.single(GpiObservationMode.applyItem,
                             obsModeLUT.getOrElse(m, UNKNOWN_SETTING)),
      params => {
        Configuration.single(GpiPPM.applyItem,
                             apodizerLUT.getOrElse(params.apodizer,
                                                   UNKNOWN_SETTING)) |+|
          Configuration.single(
            GpiFPM.applyItem,
            fpmLUT.getOrElse(params.fpm, UNKNOWN_SETTING)) |+|
          Configuration.single(
            GpiLyot.applyItem,
            lyotLUT.getOrElse(params.lyot, UNKNOWN_SETTING)) |+|
          Configuration.single(GpiIFSFilter.applyItem,
                               params.filter.displayValue)
      }
    )

  private def gpiConfiguration(config: GpiConfig): Configuration =
    config match {
      case r: RegularGpiConfig => regularGpiConfiguration(r)
      case AlignAndCalibConfig => AlignAndCalibConfig.config
    }

  private def regularGpiConfiguration(config: RegularGpiConfig): Configuration =
    Configuration.single(GpiAdc.applyItem,
      (config.adc === LegacyAdc.IN)
        .fold(1, 0)) |+|
      Configuration.single(GpiUseAo.applyItem,
        config.aoFlags.useAo
          .fold(1, 0)) |+|
      Configuration.single(GpiUseCal.applyItem,
        config.aoFlags.useCal
          .fold(1, 0)) |+|
      Configuration.single(GpiFpmPinholeBias.applyItem,
        config.aoFlags.alignFpm
          .fold(1, 0)) |+|
      Configuration.single(GpiAoOptimize.applyItem,
        config.aoFlags.aoOptimize
          .fold(1, 0)) |+|
      Configuration.single(GpiIntegrationTime.applyItem,
        config.expTime.toMillis / 1000.0) |+|
      Configuration.single(GpiNumCoadds.applyItem,   config.coAdds) |+|
      Configuration.single(GpiIFSReadMode.applyItem, config.readMode.value) |+|
      Configuration.single(GpiIFSStartX.applyItem,   config.area.startX) |+|
      Configuration.single(GpiIFSStartY.applyItem,   config.area.startY) |+|
      Configuration.single(GpiIFSEndX.applyItem,     config.area.endX) |+|
      Configuration.single(GpiIFSEndY.applyItem,     config.area.endY) |+|
      Configuration.single(GpiMagI.applyItem,        config.aoFlags.magI) |+|
      Configuration.single(GpiMagH.applyItem,        config.aoFlags.magH) |+|
      Configuration.single(
        GpiCalEntranceShutter.applyItem,
        (config.shutters.calEntranceShutter === LegacyShutter.OPEN)
          .fold(1, 0)) |+|
      Configuration.single(
        GpiCalReferenceShutter.applyItem,
        (config.shutters.calReferenceShutter === LegacyShutter.OPEN)
          .fold(1, 0)) |+|
      Configuration.single(
        GpiCalScienceShutter.applyItem,
        (config.shutters.calScienceShutter === LegacyShutter.OPEN)
          .fold(1, 0)) |+|
      Configuration.single(
        GpiEntranceShutter.applyItem,
        (config.shutters.entranceShutter === LegacyShutter.OPEN)
          .fold(1, 0)) |+|
      Configuration.single(GpiPupilCamera.applyItem,
                           (config.pc === LegacyPupilCamera.IN)
                             .fold(1, 0)) |+|
      Configuration.single(GpiSCAttenuation.applyItem, config.asu.attenuation) |+|
      Configuration.single(GpiSCPower.applyItem,
                           (config.asu.sc === LegacyArtificialSource.ON)
                             .fold(100.0, 0.0)) |+|
      Configuration.single(GpiSrcVis.applyItem,
                           (config.asu.vis === LegacyArtificialSource.ON)
                             .fold(1, 0)) |+|
      Configuration.single(GpiSrcIR.applyItem,
                           (config.asu.ir === LegacyArtificialSource.ON)
                             .fold(1, 0)) |+|
      Configuration.single(GpiPolarizerDeplay.applyItem,
                           (config.disperser === LegacyDisperser.WOLLASTON)
                             .fold(1, 0)) |+|
      (if (config.disperser === LegacyDisperser.WOLLASTON)
         Configuration.single(GpiPolarizerAngle.applyItem,
                              config.disperserAngle)
       else Configuration.Zero) |+|
      obsModeConfiguration(config)

  private def computeRegularConfig[F[_]: Monad: Logger](
    client: GpiClient[F],
    config: RegularGpiConfig
  ): F[Configuration] =
    for {
      b <- gpiConfiguration(config).pure[F]
      c <- GpiStatusApply.overridePolAngle(client.statusDb, b)
      q <- GpiStatusApply.foldConfig(client.statusDb, c)
      p <- GpiStatusApply.overrideObsMode(client.statusDb, config, q)
      _ <- Logger[F].info(s"Applied GPI config ${p.config}").unlessA(p.config.isEmpty)
    } yield p

  def apply[F[_]: Sync: Logger](
    client: GpiClient[F],
    gds:    GdsClient[F]
  ): GpiController[F] =
    new AbstractGiapiInstrumentController[F, GpiConfig, GpiClient[F]](client)
    with GpiController[F] {
      private implicit val eqResponse: Eq[Response] = Eq.fromUniversalEquals
      override val gdsClient: GdsClient[F]          = gds

      override val name = "GPI"

      override def alignAndCalib: F[Unit] =
        client.alignAndCalib
          .ensure(SeqexecFailure.Execution("Failure executing Align And Calib"))(
            _.response =!= Response.ERROR)
          .adaptError {
            case CommandResultException(_, message) =>
              SeqexecFailure.Execution(message)
          }
          .void

      override def configuration(config: GpiConfig): F[Configuration] =
        config match {
          case c: RegularGpiConfig => computeRegularConfig(client, c)
          case AlignAndCalibConfig => AlignAndCalibConfig.config.pure[F]
        }

      override def statusDb: GiapiStatusDb[F] = client.statusDb
    }

}
