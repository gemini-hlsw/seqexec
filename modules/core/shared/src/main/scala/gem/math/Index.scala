// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.{ Order, Show }
import cats.instances.short._
import mouse.boolean._
import gem.syntax.string._

/** A positive, non-zero value for numbered identifiers. */
sealed abstract case class Index(toShort: Short) {
  def format: String =
    s"$toShort"
}

object Index {

  val One: Index =
    unsafeFromShort(1)

  def fromShort(i: Short): Option[Index] =
    (i > 0) option new Index(i) {}

  def unsafeFromShort(i: Short): Index =
    fromShort(i).getOrElse(sys.error(s"Negative index: $i"))

  def fromString(s: String): Option[Index] =
    s.parseShortOption.filter(_ > 0).map(new Index(_) {})

  def unsafeFromString(s: String): Index =
    fromString(s).getOrElse(sys.error(s"Malformed observation index: '$s'"))

  implicit val OrderIndex: Order[Index] =
    Order.by(_.toShort)

  implicit val OrderingIndex: scala.math.Ordering[Index] =
    OrderIndex.toOrdering

  implicit val showIndex: Show[Index] =
    Show.fromToString

}