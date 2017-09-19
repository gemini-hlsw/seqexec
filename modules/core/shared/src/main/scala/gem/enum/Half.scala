// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.syntax.eq._
import cats.instances.string._
import gem.util.Enumerated

/**
 * Enumerated type for semester half.
 * @group Enumerations (Generated)
 */
sealed abstract class Half(
  val tag: String,
  val toInt: Int
) extends Product with Serializable

object Half {

  /** @group Constructors */ case object A extends Half("A", 0)
  /** @group Constructors */ case object B extends Half("B", 1)

  /** All members of Half, in canonical order. */
  val all: List[Half] =
    List(A, B)

  /** Select the member of Half with the given tag, if any. */
  def fromTag(s: String): Option[Half] =
    all.find(_.tag === s)

  /** Select the member of Half with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): Half =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val HalfEnumerated: Enumerated[Half] =
    new Enumerated[Half] {
      def all = Half.all
      def tag(a: Half) = a.tag
    }

}