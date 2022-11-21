// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package ocs2
package config

import java.time.Duration

import scala.collection.immutable.SortedSet

import cats.Eq
import cats.data.NonEmptySet
import cats.syntax.all._
import io.chrisdavenport.cats.time.instances.all._
import lucuma.core.enums.GcalArc
import lucuma.core.enums.GcalContinuum
import lucuma.core.enums.GcalDiffuser
import lucuma.core.enums.GcalFilter
import lucuma.core.enums.GcalShutter
import monocle._
import monocle.std.either.stdLeft
import monocle.std.either.stdRight
import ocs2.CoAdds

import GcalConfig.{ GcalArcs, GcalLamp }

/**
 * Additional configuration information for Gcal steps.
 * @group Configurations
 */
final case class GcalConfig(
  lamp:         GcalLamp,
  filter:       GcalFilter,
  diffuser:     GcalDiffuser,
  shutter:      GcalShutter,
  exposureTime: Duration,
  coadds:       CoAdds
) {

  def continuum: Option[GcalContinuum] =
    lamp.swap.toOption

  def arcs: Set[GcalArc] =
    lamp.fold(_ => Set.empty[GcalArc], _.toSet)
}

object GcalConfig extends GcalConfigOptics {

  final case class GcalArcs(arcs: NonEmptySet[GcalArc]) {
    def toList: List[GcalArc] =
      arcs.toList

    def toSet: SortedSet[GcalArc] =
      arcs.toSortedSet
  }

  object GcalArcs extends GcalArcsOptics {

    def of(arc0: GcalArc, arcs: GcalArc*): GcalArcs =
      GcalArcs(NonEmptySet.of(arc0, arcs: _*))

    implicit val EqGcalArcs: Eq[GcalArcs] =
      Eq.by(_.arcs)
  }

  trait GcalArcsOptics {

    /** @group Optics */
    val arcs: Lens[GcalArcs, NonEmptySet[GcalArc]] =
      Lens[GcalArcs, NonEmptySet[GcalArc]](_.arcs)(a => _.copy(arcs = a))

  }

  type GcalLamp = Either[GcalContinuum, GcalArcs]

  object GcalLamp {
    def fromConfig(
      continuum: Option[GcalContinuum],
      arcs:      (GcalArc, Boolean)*
    ): Option[GcalLamp] = {
      // Extract the arc lamps to include, if any.
      val as = arcs.filter(_._2).unzip._1.toList

      // Extract the continuum lamp, assuming there are no arcs.
      val co = continuum.flatMap(c => if (as.isEmpty) Some(Left(c)) else None)

      // Prepare the arc lamps, assuming there is no continuum.
      val ao = as match {
        case h :: t if continuum.isEmpty => Some(Right(GcalArcs.of(h, t: _*)))
        case _                           => None
      }

      co.orElse(ao)
    }

    def fromContinuum(continuum: GcalContinuum): GcalLamp =
      Left(continuum)

    def fromArcs(arc0: GcalArc, arcs: GcalArc*): GcalLamp =
      Right(GcalArcs.of(arc0, arcs: _*))

    def unsafeFromConfig(continuum: Option[GcalContinuum], arcs: (GcalArc, Boolean)*): GcalLamp =
      fromConfig(continuum, arcs: _*).getOrElse {
        sys.error(
          s"misconfigured Gcal lamps: continuum=$continuum, arcs=[${arcs.filter(_._2).unzip._1.mkString(",")}]"
        )
      }
  }

  implicit val GcalConfigEq: Eq[GcalConfig] =
    Eq.by(g => (g.lamp, g.filter, g.diffuser, g.shutter, g.exposureTime, g.coadds))

}

trait GcalConfigOptics {

  /** @group Optics */
  val lamp: Lens[GcalConfig, GcalLamp] =
    Lens[GcalConfig, GcalLamp](_.lamp)(a => _.copy(lamp = a))

  /** @group Optics */
  val filter: Lens[GcalConfig, GcalFilter] =
    Lens[GcalConfig, GcalFilter](_.filter)(a => _.copy(filter = a))

  /** @group Optics */
  val diffuser: Lens[GcalConfig, GcalDiffuser] =
    Lens[GcalConfig, GcalDiffuser](_.diffuser)(a => _.copy(diffuser = a))

  /** @group Optics */
  val shutter: Lens[GcalConfig, GcalShutter] =
    Lens[GcalConfig, GcalShutter](_.shutter)(a => _.copy(shutter = a))

  /** @group Optics */
  val exposureTime: Lens[GcalConfig, Duration] =
    Lens[GcalConfig, Duration](_.exposureTime)(a => _.copy(exposureTime = a))

  /** @group Optics */
  val coadds: Lens[GcalConfig, CoAdds] =
    Lens[GcalConfig, CoAdds](_.coadds)(a => _.copy(coadds = a))

  /** @group Optics */
  val continuum: Optional[GcalConfig, GcalContinuum] =
    lamp.andThen(stdLeft[GcalContinuum, GcalArcs])

  /** @group Optics */
  val arcs: Optional[GcalConfig, NonEmptySet[GcalArc]] =
    lamp.andThen(stdRight[GcalContinuum, GcalArcs]).andThen(GcalArcs.arcs)

}
