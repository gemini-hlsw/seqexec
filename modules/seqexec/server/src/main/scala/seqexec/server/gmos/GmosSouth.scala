// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gmos

import cats.effect.Sync
import cats.implicits._
import seqexec.model.enum.Instrument
import seqexec.server.ConfigUtilOps
import seqexec.server.ConfigUtilOps._
import seqexec.server.gmos.Gmos.SiteSpecifics
import seqexec.server.gmos.GmosController.{GmosSouthController, SouthTypes, southConfigTypes}
import seqexec.server.keywords.DhsClient
import seqexec.server.tcs.FOCAL_PLANE_SCALE
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.gemini.gmos.GmosSouthType
import edu.gemini.spModel.gemini.gmos.GmosSouthType.FPUnitSouth._
import edu.gemini.spModel.gemini.gmos.InstGmosCommon.{FPU_PROP_NAME, STAGE_MODE_PROP}
import edu.gemini.spModel.gemini.gmos.InstGmosSouth._
import edu.gemini.spModel.seqcomp.SeqConfigNames.INSTRUMENT_KEY
import squants.Length
import squants.space.Arcseconds

final case class GmosSouth[F[_]: Sync](c: GmosSouthController[F], dhsClient: DhsClient[F]) extends Gmos[F, SouthTypes](c,
  new SiteSpecifics[SouthTypes] {
    override val fpuDefault: GmosSouthType.FPUnitSouth = FPU_NONE
    override def extractFilter(config: Config): Either[ConfigUtilOps.ExtractFailure, SouthTypes#Filter] =
      config.extractAs[SouthTypes#Filter](INSTRUMENT_KEY / FILTER_PROP)
    override def extractDisperser(config: Config): Either[ConfigUtilOps.ExtractFailure, GmosSouthType.DisperserSouth] =
      config.extractAs[SouthTypes#Disperser](INSTRUMENT_KEY / DISPERSER_PROP)
    override def extractFPU(config: Config): Either[ConfigUtilOps.ExtractFailure, GmosSouthType.FPUnitSouth] =
      config.extractAs[SouthTypes#FPU](INSTRUMENT_KEY / FPU_PROP_NAME)
    override def extractStageMode(config: Config): Either[ConfigUtilOps.ExtractFailure, GmosSouthType.StageModeSouth] =
      config.extractAs[SouthTypes#GmosStageMode](INSTRUMENT_KEY / STAGE_MODE_PROP)
  })(southConfigTypes) {
  override val resource: Instrument = Instrument.GmosS
  override val dhsInstrumentName: String = "GMOS-S"

  // TODO Use different value if using electronic offsets
  override val oiOffsetGuideThreshold: Option[Length] = (Arcseconds(0.01)/FOCAL_PLANE_SCALE).some
}

object GmosSouth {
  val name: String = INSTRUMENT_NAME_PROP

  def apply[F[_]: Sync](c: GmosController[F, SouthTypes], dhsClient: DhsClient[F]): GmosSouth[F] = new GmosSouth[F](c, dhsClient)
}
