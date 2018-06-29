// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package config

import cats.Eq
import cats.data.NonEmptySet
import cats.implicits._
import gem.CoAdds
import gem.enum.{GcalArc, GcalContinuum, GcalDiffuser, GcalFilter, GcalShutter}
import java.time.Duration
import scala.collection.immutable.SortedSet
import GcalConfig.GcalLamp

/**
 * Additional configuration information for [[gem.Step.Gcal Gcal]] steps.
 * @group Configurations
 */
final case class GcalConfig(lamp: GcalLamp, filter: GcalFilter, diffuser: GcalDiffuser, shutter: GcalShutter, exposureTime: Duration, coadds: CoAdds) {
  def continuum: Option[GcalContinuum] =
    lamp.swap.toOption

  def arcs: Set[GcalArc] =
    lamp.fold(_ => Set.empty[GcalArc], _.toSet)
}

object GcalConfig {

  final case class GcalArcs(arcs: NonEmptySet[GcalArc]) {
    def toList: List[GcalArc] =
      arcs.toList

    def toSet: SortedSet[GcalArc] =
      arcs.toSortedSet
  }

  object GcalArcs {
    def of(arc0: GcalArc, arcs: GcalArc*): GcalArcs =
      GcalArcs(NonEmptySet.of(arc0, arcs: _*))
  }

  type GcalLamp = Either[GcalContinuum, GcalArcs]

  object GcalLamp {
    def fromConfig(continuum: Option[GcalContinuum], arcs: (GcalArc, Boolean)*): Option[GcalLamp] = {
      // Extract the arc lamps to include, if any.
      val as = arcs.filter(_._2).unzip._1.toList

      // Extract the continuum lamp, assuming there are no arcs.
      val co = continuum.flatMap { c => if (as.isEmpty) Some(Left(c)) else None }

      // Prepare the arc lamps, assuming there is no continuum.
      val ao = as match {
        case h :: t if continuum.isEmpty => Some(Right(GcalArcs.of(h, t: _*)))
        case _                           => None
      }

      co orElse ao
    }

    def fromContinuum(continuum: GcalContinuum): GcalLamp =
      Left(continuum)

    def fromArcs(arc0: GcalArc, arcs: GcalArc*): GcalLamp =
      Right(GcalArcs.of(arc0, arcs: _*))

    def unsafeFromConfig(continuum: Option[GcalContinuum], arcs: (GcalArc, Boolean)*): GcalLamp =
      fromConfig(continuum, arcs: _*).getOrElse {
        sys.error(s"misconfigured Gcal lamps: continuum=$continuum, arcs=[${arcs.filter(_._2).unzip._1.mkString(",")}]")
      }
  }

  implicit val GcalConfigEq: Eq[GcalConfig] =
    Eq.fromUniversalEquals // TODO: double-check

}
