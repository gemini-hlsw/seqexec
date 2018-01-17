// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.Eq

import gem.enum.Site
import gem.math.Ephemeris

import monocle.Optional
import monocle.macros.Lenses

/** A target of observation. */
@Lenses final case class Target(name: String, track: Track)

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object Target {

  val ephemerides: Optional[Target, Map[Site, Ephemeris]] =
    track composeOptional Track.ephemerides

  implicit val EqTarget: Eq[Target] =
    Eq.fromUniversalEquals
}
