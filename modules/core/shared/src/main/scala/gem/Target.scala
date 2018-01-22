// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.Eq

import gem.math.ProperMotion

import monocle.macros.Lenses

/** A target of observation. */
@Lenses final case class Target(name: String, track: Either[EphemerisKey, ProperMotion])

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object Target {

  implicit val EqTarget: Eq[Target] =
    Eq.fromUniversalEquals
}
