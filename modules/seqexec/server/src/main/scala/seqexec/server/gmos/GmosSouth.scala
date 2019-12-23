// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gmos

import cats.implicits._
import cats.effect.Concurrent
import cats.effect.Timer
import cats.effect.concurrent.Ref
import io.chrisdavenport.log4cats.Logger
import seqexec.model.enum.Instrument
import seqexec.server.{CleanConfig, ConfigUtilOps}
import seqexec.server.CleanConfig.extractItem
import seqexec.server.ConfigUtilOps._
import seqexec.server.gmos.Gmos.SiteSpecifics
import seqexec.server.gmos.GmosController.{SouthTypes, southConfigTypes}
import seqexec.server.keywords.DhsClient
import seqexec.server.tcs.FOCAL_PLANE_SCALE
import edu.gemini.spModel.gemini.gmos.GmosSouthType
import edu.gemini.spModel.gemini.gmos.GmosSouthType.FPUnitSouth._
import edu.gemini.spModel.gemini.gmos.InstGmosCommon.{FPU_PROP_NAME, STAGE_MODE_PROP}
import edu.gemini.spModel.gemini.gmos.InstGmosSouth._
import squants.Length
import squants.space.Arcseconds

final case class GmosSouth[F[_]: Concurrent: Timer: Logger] private (
 c: GmosSouthController[F],
 dhsClient: DhsClient[F],
 nsCmdR: Ref[F, Option[NSObserveCommand]]
) extends Gmos[F, SouthTypes](
  c,
  new SiteSpecifics[SouthTypes] {
    override val fpuDefault: GmosSouthType.FPUnitSouth = FPU_NONE
    override def extractFilter(config: CleanConfig): Either[ConfigUtilOps.ExtractFailure, SouthTypes#Filter] =
      config.extractInstAs[SouthTypes#Filter](FILTER_PROP)
    override def extractDisperser(config: CleanConfig): Either[ConfigUtilOps.ExtractFailure, GmosSouthType.DisperserSouth] =
      config.extractInstAs[SouthTypes#Disperser](DISPERSER_PROP)
    override def extractFPU(config: CleanConfig): Either[ConfigUtilOps.ExtractFailure, GmosSouthType.FPUnitSouth] =
      config.extractInstAs[SouthTypes#FPU](FPU_PROP_NAME)
    override def extractStageMode(config: CleanConfig): Either[ConfigUtilOps.ExtractFailure, GmosSouthType.StageModeSouth] =
      config.extractInstAs[SouthTypes#GmosStageMode](STAGE_MODE_PROP)
  },
  nsCmdR
)(southConfigTypes) {
  override val resource: Instrument = Instrument.GmosS
  override val dhsInstrumentName: String = "GMOS-S"

  // TODO Use different value if using electronic offsets
  override val oiOffsetGuideThreshold: Option[Length] = (Arcseconds(0.01)/FOCAL_PLANE_SCALE).some
}

object GmosSouth {
  val name: String = INSTRUMENT_NAME_PROP

  def apply[F[_]: Concurrent: Timer: Logger](
    c: GmosController[F, SouthTypes],
    dhsClient: DhsClient[F],
    nsCmdR: Ref[F, Option[NSObserveCommand]]
  ): GmosSouth[F] = new GmosSouth[F](c, dhsClient, nsCmdR)
}
