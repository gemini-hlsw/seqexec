package gem

import gem.config.InstrumentConfig

import scalaz.\/

object SmartGcal {

  sealed trait ExpansionError extends Product with Serializable

  type ExpansionResult[A] = ExpansionError \/ A
  type ExpandedSteps      = List[GcalStep[InstrumentConfig]]
  type LookupResult       = ExpansionResult[ExpandedSteps]

  final case class StepNotFound(loc: Location.Middle) extends ExpansionError
  case object      NotSmartGcal                       extends ExpansionError
  case object      NoMappingDefined                   extends ExpansionError

  def stepNotFound(loc: Location.Middle): ExpansionError = StepNotFound(loc)
  val notSmartGcal: ExpansionError                       = NotSmartGcal
  val noMappingDefined: ExpansionError                   = NoMappingDefined
}
