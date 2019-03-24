// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import gem.util.Location

/**
 * Module of types and constuctors related to Smart GCal.
 * @group Sequence Model
 */
object SmartGcal {

  sealed trait ExpansionError extends Product with Serializable

  type ExpandedSteps = List[Step]

  final case class StepNotFound(loc: Location.Middle) extends ExpansionError
  case object      NotSmartGcal                       extends ExpansionError
  case object      NoMappingDefined                   extends ExpansionError

  def stepNotFound(loc: Location.Middle): ExpansionError = StepNotFound(loc)
  val notSmartGcal: ExpansionError                       = NotSmartGcal
  val noMappingDefined: ExpansionError                   = NoMappingDefined
}
