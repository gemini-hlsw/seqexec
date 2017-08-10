package edu.gemini.seqexec.server

import edu.gemini.seqexec.server.ConfigUtilOps._
import edu.gemini.seqexec.server.Gmos.SiteSpecifics
import edu.gemini.seqexec.server.GmosController.{GmosSouthController, SouthTypes, southConfigTypes}
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.gemini.gmos.GmosSouthType
import edu.gemini.spModel.gemini.gmos.GmosSouthType.FPUnitSouth._
import edu.gemini.spModel.gemini.gmos.InstGmosSouth._
import edu.gemini.spModel.seqcomp.SeqConfigNames.INSTRUMENT_KEY
import edu.gemini.spModel.gemini.gmos.InstGmosCommon.{FPU_PROP_NAME, STAGE_MODE_PROP}

import scalaz.\/

final class GmosSouth(c: GmosSouthController) extends Gmos[SouthTypes](c,
  new SiteSpecifics[SouthTypes] {
    override val fpuDefault: GmosSouthType.FPUnitSouth = FPU_NONE
    override def extractFilter(config: Config): \/[ConfigUtilOps.ExtractFailure, SouthTypes#Filter] = config.extract(INSTRUMENT_KEY / FILTER_PROP).as[SouthTypes#Filter]
    override def extractDisperser(config: Config): \/[ConfigUtilOps.ExtractFailure, GmosSouthType.DisperserSouth] = config.extract(INSTRUMENT_KEY / DISPERSER_PROP).as[SouthTypes#Disperser]
    override def extractFPU(config: Config): \/[ConfigUtilOps.ExtractFailure, GmosSouthType.FPUnitSouth] = config.extract(INSTRUMENT_KEY / FPU_PROP_NAME).as[SouthTypes#FPU]
    override def extractStageMode(config: Config): \/[ConfigUtilOps.ExtractFailure, GmosSouthType.StageModeSouth] = config.extract(INSTRUMENT_KEY / STAGE_MODE_PROP).as[SouthTypes#GmosStageMode]
  })(southConfigTypes) {
  override val name: String = "GMOS-S"
  override val dhsInstrumentName: String = "GMOS-S"
}

object GmosSouth {
  val name: String = INSTRUMENT_NAME_PROP

  def apply(c: GmosController[SouthTypes]): GmosSouth = new GmosSouth(c)
}