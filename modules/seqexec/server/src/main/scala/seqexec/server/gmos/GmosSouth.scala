// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gmos

import seqexec.model.enum.{ Instrument, Resource }
import seqexec.server.ConfigUtilOps
import seqexec.server.ConfigUtilOps._
import seqexec.server.gmos.Gmos.SiteSpecifics
import seqexec.server.gmos.GmosController.{GmosSouthController, SouthTypes, southConfigTypes}
import seqexec.server.keywords.DhsClient
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.gemini.gmos.GmosSouthType
import edu.gemini.spModel.gemini.gmos.GmosSouthType.FPUnitSouth._
import edu.gemini.spModel.gemini.gmos.InstGmosCommon.{FPU_PROP_NAME, STAGE_MODE_PROP}
import edu.gemini.spModel.gemini.gmos.InstGmosSouth._
import edu.gemini.spModel.seqcomp.SeqConfigNames.INSTRUMENT_KEY

final case class GmosSouth(c: GmosSouthController, dhsClient: DhsClient) extends Gmos[SouthTypes](c,
  new SiteSpecifics[SouthTypes] {
    override val fpuDefault: GmosSouthType.FPUnitSouth = FPU_NONE
    override def extractFilter(config: Config): Either[ConfigUtilOps.ExtractFailure, SouthTypes#Filter] = config.extractAs[SouthTypes#Filter](INSTRUMENT_KEY / FILTER_PROP)
    override def extractDisperser(config: Config): Either[ConfigUtilOps.ExtractFailure, GmosSouthType.DisperserSouth] = config.extractAs[SouthTypes#Disperser](INSTRUMENT_KEY / DISPERSER_PROP)
    override def extractFPU(config: Config): Either[ConfigUtilOps.ExtractFailure, GmosSouthType.FPUnitSouth] = config.extractAs[SouthTypes#FPU](INSTRUMENT_KEY / FPU_PROP_NAME)
    override def extractStageMode(config: Config): Either[ConfigUtilOps.ExtractFailure, GmosSouthType.StageModeSouth] = config.extractAs[SouthTypes#GmosStageMode](INSTRUMENT_KEY / STAGE_MODE_PROP)
  })(southConfigTypes) {
  override val resource: Resource = Instrument.GmosS
  override val dhsInstrumentName: String = "GMOS-S"
}

object GmosSouth {
  val name: String = INSTRUMENT_NAME_PROP

  def apply(c: GmosController[SouthTypes], dhsClient: DhsClient): GmosSouth = new GmosSouth(c, dhsClient)
}
