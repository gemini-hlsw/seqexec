// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server

import edu.gemini.seqexec.server.ConfigUtilOps._
import edu.gemini.seqexec.server.Gmos.SiteSpecifics
import edu.gemini.seqexec.server.GmosController.{GmosNorthController, NorthTypes, northConfigTypes}
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.gemini.gmos.GmosNorthType
import edu.gemini.spModel.gemini.gmos.GmosNorthType.FPUnitNorth._
import edu.gemini.spModel.gemini.gmos.InstGmosNorth._
import edu.gemini.spModel.seqcomp.SeqConfigNames.INSTRUMENT_KEY
import edu.gemini.spModel.gemini.gmos.InstGmosCommon.{FPU_PROP_NAME, STAGE_MODE_PROP}

import scalaz.\/

/**
  * Created by jluhrs on 8/10/17.
  */

final class GmosNorth(c: GmosNorthController) extends Gmos[NorthTypes](c,
  new SiteSpecifics[NorthTypes] {
    override val fpuDefault: GmosNorthType.FPUnitNorth = FPU_NONE
    override def extractFilter(config: Config): \/[ConfigUtilOps.ExtractFailure, NorthTypes#Filter] = config.extract(INSTRUMENT_KEY / FILTER_PROP).as[NorthTypes#Filter]
    override def extractDisperser(config: Config): \/[ConfigUtilOps.ExtractFailure, GmosNorthType.DisperserNorth] = config.extract(INSTRUMENT_KEY / DISPERSER_PROP).as[NorthTypes#Disperser]
    override def extractFPU(config: Config): \/[ConfigUtilOps.ExtractFailure, GmosNorthType.FPUnitNorth] = config.extract(INSTRUMENT_KEY / FPU_PROP_NAME).as[NorthTypes#FPU]
    override def extractStageMode(config: Config): \/[ConfigUtilOps.ExtractFailure, GmosNorthType.StageModeNorth] = config.extract(INSTRUMENT_KEY / STAGE_MODE_PROP).as[NorthTypes#GmosStageMode]
  })(northConfigTypes) {
  override val name: String = "GMOS-N"
  override val dhsInstrumentName: String = "GMOS-N"
}

object GmosNorth {
  val name: String = INSTRUMENT_NAME_PROP

  def apply(c: GmosController[NorthTypes]): GmosNorth = new GmosNorth(c)
}