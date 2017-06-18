package gem
package config

import gem.enum.{GcalArc, GcalContinuum, GcalDiffuser, GcalFilter, GcalShutter}

import java.time.Duration

import scalaz._
import Scalaz._

import GcalConfig.GcalLamp

case class GcalConfig(lamp: GcalLamp, filter: GcalFilter, diffuser: GcalDiffuser, shutter: GcalShutter, exposureTime: Duration, coadds: Short) {
  def continuum: Option[GcalContinuum] =
    lamp.swap.toOption

  def arcs: ISet[GcalArc] =
    lamp.fold(_ => ISet.empty[GcalArc], _.toISet)
}

object GcalConfig {
  sealed abstract case class GcalArcs(arcs: OneAnd[ISet, GcalArc]) {
    def toList: List[GcalArc] =
      arcs.head :: arcs.tail.toList

    def toISet: ISet[GcalArc] =
      arcs.tail.insert(arcs.head)
  }

  object GcalArcs {
    def apply(arc0: GcalArc, arcs: List[GcalArc]): GcalArcs = {
      val all = ISet.fromList(arc0 :: arcs)
      new GcalArcs(OneAnd(all.elemAt(0).get, all.deleteAt(0))) {}
    }
  }

//  type GcalArcs = OneAnd[ISet, GcalArc]
  type GcalLamp = GcalContinuum \/ GcalArcs

  object GcalLamp {
    def fromConfig(continuum: Option[GcalContinuum], arcs: (GcalArc, Boolean)*): Option[GcalLamp] = {
      // Extract the arc lamps to include, if any.
      val as = arcs.filter(_._2).unzip._1.toList

      // Extract the continuum lamp, assuming there are no arcs.
      val co = continuum.flatMap { c => as.isEmpty option c.left[GcalArcs] }

      // Prepare the arc lamps, assuming there is no continuum.
      val ao = as match {
        case h :: t if continuum.isEmpty => Some(GcalArcs(h, t).right[GcalContinuum])
        case _                           => None
      }

      co orElse ao
    }

    def fromContinuum(continuum: GcalContinuum): GcalLamp =
      continuum.left

    def fromArcs(arc0: GcalArc, arcs: GcalArc*): GcalLamp =
      GcalArcs(arc0, arcs.toList).right

    def unsafeFromConfig(continuum: Option[GcalContinuum], arcs: (GcalArc, Boolean)*): GcalLamp =
      fromConfig(continuum, arcs: _*).getOrElse {
        sys.error(s"misconfigured Gcal lamps: continuum=$continuum, arcs=[${arcs.filter(_._2).unzip._1.mkString(",")}]")
      }
  }
}
