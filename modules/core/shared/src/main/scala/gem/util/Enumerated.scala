// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.util

import cats.Order
import cats.implicits._

/**
 * Typeclass for an enumerated type with unique string tags and a canonical ordering.
 * @group Typeclasses
 */
trait Enumerated[A] extends Order[A] {

  /** All members of this enumeration, in unspecified but canonical order. */
  def all: List[A]

  /** The tag for a given value. */
  def tag(a: A): String

  /** Select the member of this enumeration with the given tag, if any. */
  def fromTag(s: String): Option[A] = all.find(tag(_) === s)

  /** Select the member of this enumeration with the given tag, throwing if absent. */
  def unsafeFromTag(tag: String): A = fromTag(tag).getOrElse(sys.error("Invalid tag: " + tag))

  def compare(a: A, b: A): Int =
    Order[Int].compare(indexOfTag(tag(a)), indexOfTag(tag(b)))

  // Hashed index lookup, for efficient use as an `Order`.
  private lazy val indexOfTag: Map[String, Int] =
    all.zipWithIndex.map { case (a, n) => (tag(a), n) } (collection.breakOut)

}

object Enumerated {
  def apply[A](implicit ev: Enumerated[A]): ev.type = ev

  def of[A <: Product](a: A, as: A*): Enumerated[A] =
    new Enumerated[A] {
      def all: List[A] = a :: as.toList
      def tag(a: A): String = a.productPrefix
    }

}

/** @group Typeclasses */
trait Obsoletable[A] {
  def isActive(a: A): Boolean
  final def isObsolete(a: A): Boolean = !isActive(a)
}


/**
 * Typeclass for things that can be shown in a user interface.
 * @group Typeclasses
 */
trait Display[A] {
  def name(a:A): String         // short name, for labels
  def elaboration(a:A): Option[String]  // an elaboration on the name, used for computing longname
  def longName(a: A): String = name(a) + elaboration(a).fold("")(n => s" ($n)")
}
