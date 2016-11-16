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

//  implicit class ArcSetOps(as: List[GcalArc]) {
//    def toGcalArcs: Option[GcalArcs] =
//      as.headOption.map { a => OneAnd(a, ISet.fromList(as.tail)) }
//
//    def unsafeToGcalArcs: GcalArcs =
//      toGcalArcs.getOrElse(sys.error("nno Gcal arc lamps"))
//  }
//
//  def nonEmptyArcs(as: Set[GcalArc]): Option[OneAnd[Set, GcalArc]] =
//    as.headOption.map { a => OneAnd(a, as.tail) }
//
//  def unsafeNonEmptyArcs(as: Set[GcalArc]): OneAnd[Set, GcalArc] =
//    nonEmptyArcs(as).getOrElse(sys.error("no Gcal arc lamps"))

  def mkLampOption(continuum: Option[GcalContinuum], arcs: => List[(GcalArc, Boolean)]): Option[GcalLamp] =
    continuum.map(_.left[GcalArcs]) orElse {
      val as = arcs.filter(_._2).unzip._1
      as.headOption.map { a => OneAnd(a, ISet.fromList(as.tail)).right[GcalContinuum] }
    }

  def unsafeMkLamp(co: Option[GcalContinuum], as: => List[(GcalArc, Boolean)]): GcalLamp =
    mkLampOption(co, as).getOrElse(sys.error("no Gcal continuum nor arc lamps"))
}

