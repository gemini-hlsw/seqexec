package gem

import gem.config._
import gem.enum.StepType

import scalaz._, Scalaz._

sealed abstract class Step[A] extends Product with Serializable {
  def instrument: A
}

final case class BiasStep   [A](instrument: A)                             extends Step[A]
final case class DarkStep   [A](instrument: A)                             extends Step[A]
final case class GcalStep   [A](instrument: A, gcal:      GcalConfig)      extends Step[A]
final case class ScienceStep[A](instrument: A, telescope: TelescopeConfig) extends Step[A]
final case class SmartStep  [A](instrument: A, smartCal:  SmartCalConfig)  extends Step[A]
