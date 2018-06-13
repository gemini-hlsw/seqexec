// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gmos

import seqexec.model.Model
import seqexec.model.Model.Instrument
import seqexec.server.ConfigUtilOps
import seqexec.server.ConfigUtilOps._
import seqexec.server.gmos.Gmos.SiteSpecifics
import seqexec.server.gmos.GmosController.{GmosSouthController, SouthTypes, southConfigTypes}
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.gemini.gmos.GmosSouthType
import edu.gemini.spModel.gemini.gmos.GmosSouthType.FPUnitSouth._
import edu.gemini.spModel.gemini.gmos.InstGmosCommon.{FPU_PROP_NAME, STAGE_MODE_PROP}
import edu.gemini.spModel.gemini.gmos.InstGmosSouth._
import edu.gemini.spModel.seqcomp.SeqConfigNames.INSTRUMENT_KEY

final case class GmosSouth(c: GmosSouthController) extends Gmos[SouthTypes](c,
  new SiteSpecifics[SouthTypes] {
    override val fpuDefault: GmosSouthType.FPUnitSouth = FPU_NONE
    override def extractFilter(config: Config): Either[ConfigUtilOps.ExtractFailure, SouthTypes#Filter] = config.extract(INSTRUMENT_KEY / FILTER_PROP).as[SouthTypes#Filter]
    override def extractDisperser(config: Config): Either[ConfigUtilOps.ExtractFailure, GmosSouthType.DisperserSouth] = config.extract(INSTRUMENT_KEY / DISPERSER_PROP).as[SouthTypes#Disperser]
    override def extractFPU(config: Config): Either[ConfigUtilOps.ExtractFailure, GmosSouthType.FPUnitSouth] = config.extract(INSTRUMENT_KEY / FPU_PROP_NAME).as[SouthTypes#FPU]
    override def extractStageMode(config: Config): Either[ConfigUtilOps.ExtractFailure, GmosSouthType.StageModeSouth] = config.extract(INSTRUMENT_KEY / STAGE_MODE_PROP).as[SouthTypes#GmosStageMode]
  })(southConfigTypes) {
  override val resource: Model.Resource = Instrument.GmosS
  override val dhsInstrumentName: String = "GMOS-S"
}

object GmosSouth {
  val name: String = INSTRUMENT_NAME_PROP

  def apply(c: GmosController[SouthTypes]): GmosSouth = new GmosSouth(c)
}