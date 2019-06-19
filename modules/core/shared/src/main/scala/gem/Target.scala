// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats._
import cats.implicits._

import gsp.math.ProperMotion

import monocle.macros.Lenses

/** A target of observation. */
@Lenses final case class Target(name: String, track: Either[EphemerisKey, ProperMotion])

object Target {

  /** Target identifier. */
  final case class Id(toInt: Int) extends AnyVal

  object Id {
    /** Ids ordered by wrapped integer value. */
    implicit val IdOrder: Order[Id] =
      Order.by(_.toInt)
  }

  /** A target order based on tracking information.  For sidereal targets this
    * roughly means by base coordinate without applying proper motion.  For
    * non-sidereal this means by `EphemerisKey`.
    */
  implicit val TargetTrackOrder: Order[Target] =
    Order.by(t => (t.track, t.name))

  /** Targets ordered by name first and then tracking information.
    *
    * Not implicit. The implicit target order is track then name.
    */
  val TargetNameOrder: Order[Target] =
    Order.by(t => (t.name, t.track))

}
