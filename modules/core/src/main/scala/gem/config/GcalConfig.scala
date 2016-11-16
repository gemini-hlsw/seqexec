package gem
package config

import gem.enum.{GcalArc, GcalContinuum, GcalShutter}

import scalaz._
import Scalaz._

import GcalConfig.GcalLamp

case class GcalConfig(lamp: GcalLamp, shutter: GcalShutter)

object GcalConfig {
  type GcalArcs = OneAnd[ISet, GcalArc]
  type GcalLamp = GcalContinuum \/ GcalArcs

  def mkLampOption(continuum: Option[GcalContinuum], arcs: => List[(GcalArc, Boolean)]): Option[GcalLamp] =
    continuum.map(_.left[GcalArcs]) orElse {
      val as = arcs.filter(_._2).unzip._1
      as.headOption.map { a => OneAnd(a, ISet.fromList(as.tail)).right[GcalContinuum] }
    }

  def unsafeMkLamp(co: Option[GcalContinuum], as: => List[(GcalArc, Boolean)]): GcalLamp =
    mkLampOption(co, as).getOrElse(sys.error("no Gcal continuum nor arc lamps"))
}

