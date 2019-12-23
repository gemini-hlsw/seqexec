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
import seqexec.server.gmos.GmosController.{NorthTypes, northConfigTypes}
import seqexec.server.keywords.DhsClient
import seqexec.server.tcs.FOCAL_PLANE_SCALE
import edu.gemini.spModel.gemini.gmos.GmosNorthType
import edu.gemini.spModel.gemini.gmos.GmosNorthType.FPUnitNorth._
import edu.gemini.spModel.gemini.gmos.InstGmosCommon.{FPU_PROP_NAME, STAGE_MODE_PROP}
import edu.gemini.spModel.gemini.gmos.InstGmosNorth._
import squants.Length
import squants.space.Arcseconds

final case class GmosNorth[F[_]: Concurrent: Timer: Logger] private (
  c: GmosNorthController[F],
  dhsClient: DhsClient[F],
  nsCmdR: Ref[F, Option[NSObserveCommand]]
) extends Gmos[F, NorthTypes](c,
  new SiteSpecifics[NorthTypes] {
    override val fpuDefault: GmosNorthType.FPUnitNorth = FPU_NONE
    override def extractFilter(config: CleanConfig): Either[ConfigUtilOps.ExtractFailure, NorthTypes#Filter] =
      config.extractInstAs[NorthTypes#Filter](FILTER_PROP)
    override def extractDisperser(config: CleanConfig): Either[ConfigUtilOps.ExtractFailure, GmosNorthType.DisperserNorth] =
      config.extractInstAs[NorthTypes#Disperser](DISPERSER_PROP)
    override def extractFPU(config: CleanConfig): Either[ConfigUtilOps.ExtractFailure, GmosNorthType.FPUnitNorth] =
      config.extractInstAs[NorthTypes#FPU](FPU_PROP_NAME)
    override def extractStageMode(config: CleanConfig): Either[ConfigUtilOps.ExtractFailure, GmosNorthType.StageModeNorth] =
      config.extractInstAs[NorthTypes#GmosStageMode](STAGE_MODE_PROP)
  },
  nsCmdR
)(northConfigTypes) {
  override val resource: Instrument = Instrument.GmosN
  override val dhsInstrumentName: String = "GMOS-N"

  // TODO Use different value if using electronic offsets
  override val oiOffsetGuideThreshold: Option[Length] = (Arcseconds(0.01)/FOCAL_PLANE_SCALE).some
}

object GmosNorth {
  val name: String = INSTRUMENT_NAME_PROP

  def apply[F[_]: Concurrent: Timer: Logger](
    c: GmosController[F, NorthTypes],
    dhsClient: DhsClient[F],
    nsCmdR: Ref[F, Option[NSObserveCommand]]
  ): GmosNorth[F] = new GmosNorth[F](c, dhsClient, nsCmdR)
}
