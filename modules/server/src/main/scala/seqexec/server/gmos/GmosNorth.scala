// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gmos

import cats.Applicative
import cats.effect.{ Ref, Temporal }
import cats.syntax.all._
import edu.gemini.spModel.gemini.gmos.GmosNorthType
import edu.gemini.spModel.gemini.gmos.GmosNorthType.FPUnitNorth._
import edu.gemini.spModel.gemini.gmos.InstGmosCommon.FPU_PROP_NAME
import edu.gemini.spModel.gemini.gmos.InstGmosCommon.STAGE_MODE_PROP
import edu.gemini.spModel.gemini.gmos.InstGmosNorth._
import org.typelevel.log4cats.Logger
import lucuma.core.enums.LightSinkName
import seqexec.model.enum.Instrument
import seqexec.server.CleanConfig
import seqexec.server.CleanConfig.extractItem
import seqexec.server.ConfigUtilOps
import seqexec.server.ConfigUtilOps._
import seqexec.server.InstrumentSpecifics
import seqexec.server.SeqexecFailure
import seqexec.server.StepType
import seqexec.server.gmos.Gmos.SiteSpecifics
import seqexec.server.gmos.GmosController.NorthTypes
import seqexec.server.gmos.GmosController.northConfigTypes
import seqexec.server.keywords.{ DhsClient, DhsClientProvider }
import seqexec.server.tcs.FOCAL_PLANE_SCALE
import squants.Length
import squants.space.Arcseconds

final case class GmosNorth[F[_]: Temporal: Logger] private (
  c:                 GmosNorthController[F],
  dhsClientProvider: DhsClientProvider[F],
  nsCmdR:            Ref[F, Option[NSObserveCommand]]
) extends Gmos[F, NorthTypes](
      c,
      new SiteSpecifics[NorthTypes] {
        def extractFilter(
          config: CleanConfig
        ): Either[ConfigUtilOps.ExtractFailure, NorthTypes#Filter] =
          config.extractInstAs[NorthTypes#Filter](FILTER_PROP)

        def extractDisperser(
          config: CleanConfig
        ): Either[ConfigUtilOps.ExtractFailure, GmosNorthType.DisperserNorth] =
          config.extractInstAs[NorthTypes#Disperser](DISPERSER_PROP)

        def extractFPU(
          config: CleanConfig
        ): Either[ConfigUtilOps.ExtractFailure, GmosNorthType.FPUnitNorth] =
          config.extractInstAs[NorthTypes#FPU](FPU_PROP_NAME)

        def extractStageMode(
          config: CleanConfig
        ): Either[ConfigUtilOps.ExtractFailure, GmosNorthType.StageModeNorth] =
          config.extractInstAs[NorthTypes#GmosStageMode](STAGE_MODE_PROP)

        val fpuDefault: GmosNorthType.FPUnitNorth = FPU_NONE

        def isCustomFPU(config: CleanConfig): Boolean =
          (extractFPU(config), extractCustomFPU(config)) match {
            case (Right(builtIn), Right(_)) => builtIn.isCustom
            case (_, Right(_))              => true
            case _                          => false
          }
      },
      nsCmdR
    )(
      northConfigTypes
    ) {
  override val resource: Instrument      = Instrument.GmosN
  override val dhsInstrumentName: String = "GMOS-N"
  override val dhsClient: DhsClient[F]   = dhsClientProvider.dhsClient(dhsInstrumentName)
  override val sequenceComplete: F[Unit] = Applicative[F].unit

}

object GmosNorth {
  val name: String = INSTRUMENT_NAME_PROP

  def apply[F[_]: Temporal: Logger](
    c:                 GmosController[F, NorthTypes],
    dhsClientProvider: DhsClientProvider[F],
    nsCmdR:            Ref[F, Option[NSObserveCommand]]
  ): GmosNorth[F] = new GmosNorth[F](c, dhsClientProvider, nsCmdR)

  object specifics extends InstrumentSpecifics {
    override val instrument: Instrument = Instrument.GmosN

    override def calcStepType(
      config:     CleanConfig,
      isNightSeq: Boolean
    ): Either[SeqexecFailure, StepType] =
      Gmos.calcStepType(instrument, config, isNightSeq)

    override def sfName(config: CleanConfig): LightSinkName = LightSinkName.Gmos

    // TODO Use different value if using electronic offsets
    override val oiOffsetGuideThreshold: Option[Length] =
      (Arcseconds(0.01) / FOCAL_PLANE_SCALE).some

  }

}
