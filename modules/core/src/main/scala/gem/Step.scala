package gem

import gem.config._
import gem.enum.StepType

import scalaz._, Scalaz._

sealed abstract class Step[I] extends Product with Serializable {
  def instrument: I
}

final case class BiasStep[I](instrument: I) extends Step[I]
final case class DarkStep[I](instrument: I) extends Step[I]
final case class GcalStep[I](instrument: I, gcal: GcalConfig) extends Step[I]
final case class ScienceStep[I](instrument: I, telescope: TelescopeConfig) extends Step[I]
final case class SmartStep[I](instrument: I, smartCal: SmartCalConfig) extends Step[I]
