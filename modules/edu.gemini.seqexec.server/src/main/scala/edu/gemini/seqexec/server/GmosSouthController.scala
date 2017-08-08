package edu.gemini.seqexec.server

trait GmosSouthController extends GmosController {
  override type Filter = edu.gemini.spModel.gemini.gmos.GmosSouthType.FilterSouth
  override type FPU = edu.gemini.spModel.gemini.gmos.GmosSouthType.FPUnitSouth
  override type GmosStageMode = edu.gemini.spModel.gemini.gmos.GmosSouthType.StageModeSouth
  override type Disperser = edu.gemini.spModel.gemini.gmos.GmosSouthType.DisperserSouth
}
