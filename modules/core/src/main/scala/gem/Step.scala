// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import gem.config._
import gem.enum.SmartGcalType

import scalaz.Functor

/**
 * An observation sequence step, parameterized on the type of its dynamic configuration, typically
 * some kind of [[gem.config.DynamicConfig DynamicConfig]] for a fully-specified step, or `Unit`
 * for a step without instrument-specific configuration information.
 * @group Sequence Model
 */
sealed abstract class Step[A] extends Product with Serializable {
  def dynamicConfig: A
}

object Step {

  final case class Bias     [A](dynamicConfig: A)                               extends Step[A]
  final case class Dark     [A](dynamicConfig: A)                               extends Step[A]
  final case class Gcal     [A](dynamicConfig: A, gcal: GcalConfig)             extends Step[A]
  final case class Science  [A](dynamicConfig: A, telescope: TelescopeConfig)   extends Step[A]
  final case class SmartGcal[A](dynamicConfig: A, smartGcalType: SmartGcalType) extends Step[A]

  implicit val FunctorStep: Functor[Step] = new Functor[Step] {
    def map[A, B](fa: Step[A])(f: A => B): Step[B] =
      fa match {
        case Bias(a)         => Bias(f(a))
        case Dark(a)         => Dark(f(a))
        case Gcal(a, g)      => Gcal(f(a), g)
        case Science(a, t)   => Science(f(a), t)
        case SmartGcal(a, s) => SmartGcal(f(a), s)
      }
  }

}
