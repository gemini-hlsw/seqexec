package gem
package config

import gem.enum.{GcalArc, GcalContinuum, GcalShutter}

import scalaz.{\/, OneAnd}

import GcalConfig.GcalLamp

case class GcalConfig(lamp: GcalLamp, shutter: GcalShutter)

object GcalConfig {
  type GcalLamp = GcalContinuum \/ OneAnd[Set, GcalArc]

  def nonEmptyArcs(as: Set[GcalArc]): Option[OneAnd[Set, GcalArc]] =
    as.headOption.map { a => OneAnd(a, as.tail) }

  def unsafeNonEmptyArcs(as: Set[GcalArc]): OneAnd[Set, GcalArc] =
    nonEmptyArcs(as).getOrElse(sys.error("no Gcal arc lamps"))
}

