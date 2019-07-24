// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gpi

import cats.Eq
import cats.Show
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
import gem.enum.GiapiStatus
import giapi.client.commands.Configuration
import giapi.client.commands.CommandResultException
import giapi.client.gpi.GpiClient
import giapi.client.gpi.GpiGuiding
import giapi.client.StatusValue
import giapi.client.GiapiException
import mouse.boolean._
import org.log4s._
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

sealed trait GpiConfig extends Product with Serializable

final case class RegularGpiConfig(
  adc:            LegacyAdc,
  expTime:        Duration,
  coAdds:         Int,
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
  val logger: Logger = getLogger

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
      Configuration.single(GpiNumCoadds.applyItem, config.coAdds) |+|
      Configuration.single(GpiMagI.applyItem, config.aoFlags.magI) |+|
      Configuration.single(GpiMagH.applyItem, config.aoFlags.magH) |+|
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

  private def computeRegularConfig[F[_]: Sync](
    client: GpiClient[F],
    config: RegularGpiConfig
  ): F[Configuration] =
    for {
      b <- Sync[F].delay(gpiConfiguration(config))
      q <- GpiStatusApply.foldConfig(client.statusDb, b)
      p <- GpiStatusApply.overrideObsMode(client.statusDb, config, q)
      _ <- Sync[F]
        .delay(logger.info(s"Applied GPI config ${p.config}"))
        .unlessA(p.config.isEmpty)
    } yield p

  def apply[F[_]: Sync](
    client: GpiClient[F],
    gds:    GdsClient[F]
  ): GpiController[F] =
    new AbstractGiapiInstrumentController[F, GpiConfig, GpiClient[F]](client)
    with GpiController[F] {
      private implicit val eqResponse: Eq[Response] = Eq.fromUniversalEquals
      override val gdsClient: GdsClient[F]          = gds

      override val name = "GPI"

      private val GuidingChannel = GiapiStatus.GpiGuiding.statusItem

      // Check if we are guiding reading from the local status db
      private def guiding: F[GpiGuiding] =
        client.statusDb
          .value(GuidingChannel)
          .map(StatusValue.intValue(_).map(x => GpiGuiding.fromInt(x.toInt)))
          .ensure(new SeqexecFailure.Execution(s"Channel $GuidingChannel should be an int"))(
            _.isDefined)
          .map { _.orNull } // orNull lets us typecheck but it will never be used due to the `ensure` call above
          .adaptError {
            case GiapiException(message) => SeqexecFailure.Execution(message) }

      override def alignAndCalib: F[Unit] =
        guiding
          .map(_ === GpiGuiding.NotGuiding)
          .ifM(
            client.alignAndCalib
              .ensure(SeqexecFailure.Execution("Failure executing Align And Calib"))(
                _.response =!= Response.ERROR)
              .adaptError {
                case CommandResultException(_, message) =>
                  SeqexecFailure.Execution(message)
              }
              .void,
            Sync[F].raiseError(
              SeqexecFailure.Execution("Cannot run align & calib while guiding")
            )
          )

      override def configuration(config: GpiConfig): F[Configuration] =
        config match {
          case c: RegularGpiConfig => computeRegularConfig(client, c)
          case AlignAndCalibConfig => AlignAndCalibConfig.config.pure[F]
        }
    }

}
