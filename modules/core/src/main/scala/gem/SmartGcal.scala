package gem

import gem.config.InstrumentConfig

object SmartGcal {

  sealed trait ExpansionError extends Product with Serializable

  type ExpandedSteps      = List[GcalStep[InstrumentConfig]]

  final case class StepNotFound(loc: Location.Middle) extends ExpansionError
  case object      NotSmartGcal                       extends ExpansionError
  case object      NoMappingDefined                   extends ExpansionError

  def stepNotFound(loc: Location.Middle): ExpansionError = StepNotFound(loc)
  val notSmartGcal: ExpansionError                       = NotSmartGcal
  val noMappingDefined: ExpansionError                   = NoMappingDefined
}
