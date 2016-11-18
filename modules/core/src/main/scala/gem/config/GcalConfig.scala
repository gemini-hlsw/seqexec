package gem
package config

import gem.enum.{GcalArc, GcalContinuum, GcalShutter}

import scalaz._
import Scalaz._

import GcalConfig.GcalLamp

case class GcalConfig(lamp: GcalLamp, shutter: GcalShutter) {
  def continuum: Option[GcalContinuum] =
    lamp.swap.toOption

  def arcs: ISet[GcalArc] =
    lamp.fold(_ => ISet.empty[GcalArc], as => as.tail.insert(as.head))
}

object GcalConfig {
  type GcalArcs = OneAnd[ISet, GcalArc]
  type GcalLamp = GcalContinuum \/ GcalArcs

  def mkLamp(continuum: Option[GcalContinuum], arcs: (GcalArc, Boolean)*): Option[GcalLamp] = {
    // Extract the arc lamps to include, if any.
    val as = arcs.filter(_._2).unzip._1.toList

    // Extract the continuum lamp, assuming there are no arcs.
    val co = continuum.flatMap { c => as.isEmpty option c.left[GcalArcs] }

    // Prepare the arc lamps, assuming there is no continuum.
    val ao = as match {
      case h :: t if continuum.isEmpty => Some(OneAnd(h, ISet.fromList(t)).right[GcalContinuum])
      case _                           => None
    }

    co orElse ao
  }

  def unsafeMkLamp(continuum: Option[GcalContinuum], arcs: (GcalArc, Boolean)*): GcalLamp =
    mkLamp(continuum, arcs: _*).getOrElse {
      sys.error(s"misconfigured Gcal lamps: continuum=$continuum, arcs=[${arcs.filter(_._2).unzip._1.mkString(",")}]")
    }
}

