// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gmos

import cats.effect.IO
import cats.implicits._
import seqexec.model.enum.Instrument
import seqexec.server.ConfigUtilOps
import seqexec.server.ConfigUtilOps._
import seqexec.server.gmos.Gmos.SiteSpecifics
import seqexec.server.gmos.GmosController.{GmosNorthController, NorthTypes, northConfigTypes}
import seqexec.server.keywords.DhsClient
import seqexec.server.tcs.FOCAL_PLANE_SCALE
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.gemini.gmos.GmosNorthType
import edu.gemini.spModel.gemini.gmos.GmosNorthType.FPUnitNorth._
import edu.gemini.spModel.gemini.gmos.InstGmosCommon.{FPU_PROP_NAME, STAGE_MODE_PROP}
import edu.gemini.spModel.gemini.gmos.InstGmosNorth._
import edu.gemini.spModel.seqcomp.SeqConfigNames.INSTRUMENT_KEY
import squants.Length
import squants.space.Arcseconds

final case class GmosNorth(c: GmosNorthController, dhsClient: DhsClient[IO]) extends Gmos[NorthTypes](c,
  new SiteSpecifics[NorthTypes] {
    override val fpuDefault: GmosNorthType.FPUnitNorth = FPU_NONE
    override def extractFilter(config: Config): Either[ConfigUtilOps.ExtractFailure, NorthTypes#Filter] =
      config.extractAs[NorthTypes#Filter](INSTRUMENT_KEY / FILTER_PROP)
    override def extractDisperser(config: Config): Either[ConfigUtilOps.ExtractFailure, GmosNorthType.DisperserNorth] =
      config.extractAs[NorthTypes#Disperser](INSTRUMENT_KEY / DISPERSER_PROP)
    override def extractFPU(config: Config): Either[ConfigUtilOps.ExtractFailure, GmosNorthType.FPUnitNorth] =
      config.extractAs[NorthTypes#FPU](INSTRUMENT_KEY / FPU_PROP_NAME)
    override def extractStageMode(config: Config): Either[ConfigUtilOps.ExtractFailure, GmosNorthType.StageModeNorth] =
      config.extractAs[NorthTypes#GmosStageMode](INSTRUMENT_KEY / STAGE_MODE_PROP)
  })(northConfigTypes) {
  override val resource: Instrument = Instrument.GmosN
  override val dhsInstrumentName: String = "GMOS-N"
  // TODO Use different value if using electronic offsets
  override val oiOffsetGuideThreshold: Option[Length] = (Arcseconds(0.01)/FOCAL_PLANE_SCALE).some
}

object GmosNorth {
  val name: String = INSTRUMENT_NAME_PROP

  def apply(c: GmosController[NorthTypes], dhsClient: DhsClient[IO]): GmosNorth = new GmosNorth(c, dhsClient)
}
