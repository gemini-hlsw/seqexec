package edu.gemini.seqexec.server

import edu.gemini.seqexec.server.ConfigUtilOps._
import edu.gemini.seqexec.server.GmosController.{CustomMaskFPU, GmosFPU, UnknownFPU}
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.gemini.gmos.GmosCommonType._
import edu.gemini.spModel.gemini.gmos.GmosSouthType
import edu.gemini.spModel.gemini.gmos.GmosSouthType.FPUnitSouth._
import edu.gemini.spModel.gemini.gmos.InstGmosSouth._
import edu.gemini.spModel.seqcomp.SeqConfigNames.INSTRUMENT_KEY
import edu.gemini.spModel.gemini.gmos.InstGmosCommon.{FPU_PROP_NAME, STAGE_MODE_PROP}

import scalaz.\/

final class GmosSouth(val c: GmosSouthController) extends Gmos[GmosSouthController](c) {
  override val name: String = "GMOS-S"
  override val dhsInstrumentName: String = "GMOS-S"

  override protected def fpuFromFPUnit(n: Option[controller.FPU], m: Option[String])(fpu: FPUnitMode): GmosFPU = fpu match {
    case FPUnitMode.BUILTIN     => c.BuiltInFPU(n.getOrElse(FPU_NONE))
    case FPUnitMode.CUSTOM_MASK => m match {
      case Some(u) => CustomMaskFPU(u)
      case _       => UnknownFPU
    }
  }

  override protected def extractFilter(config: Config): \/[ConfigUtilOps.ExtractFailure, GmosSouthType.FilterSouth] = config.extract(INSTRUMENT_KEY / FILTER_PROP).as[GmosSouthType.FilterSouth]

  override protected def extractDisperser(config: Config): \/[ExtractFailure, GmosSouthType.DisperserSouth] = config.extract(INSTRUMENT_KEY / DISPERSER_PROP).as[GmosSouthType.DisperserSouth]

  override protected def extractFPU(config: Config): \/[ExtractFailure, GmosSouthType.FPUnitSouth] = config.extract(INSTRUMENT_KEY / FPU_PROP_NAME).as[controller.FPU]

  override protected def extractStageMode(config: Config): \/[ExtractFailure, GmosSouthType.StageModeSouth] = config.extract(INSTRUMENT_KEY / STAGE_MODE_PROP).as[controller.GmosStageMode]
}

object GmosSouth {
  val name: String = INSTRUMENT_NAME_PROP

  def apply(c: GmosSouthController): GmosSouth = new GmosSouth(c)
}