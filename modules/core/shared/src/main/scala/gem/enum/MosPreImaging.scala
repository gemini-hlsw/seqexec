// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.syntax.eq._
import cats.instances.string._
import gem.util.Enumerated

/**
 * Enumerated type for MOS pre-imaging category.
 * @group Enumerations (Generated)
 */
sealed abstract class MosPreImaging(
  val tag: String,
  val description: String,
  val toBoolean: Boolean
) extends Product with Serializable

object MosPreImaging {

  /** @group Constructors */ case object IsMosPreImaging extends MosPreImaging("IsMosPreImaging", "Is MOS Pre-imaging", true)
  /** @group Constructors */ case object IsNotMosPreImaging extends MosPreImaging("IsNotMosPreImaging", "Is Not MOS Pre-Imaging", false)

  /** All members of MosPreImaging, in canonical order. */
  val all: List[MosPreImaging] =
    List(IsMosPreImaging, IsNotMosPreImaging)

  /** Select the member of MosPreImaging with the given tag, if any. */
  def fromTag(s: String): Option[MosPreImaging] =
    all.find(_.tag === s)

  /** Select the member of MosPreImaging with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): MosPreImaging =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val MosPreImagingEnumerated: Enumerated[MosPreImaging] =
    new Enumerated[MosPreImaging] {
      def all = MosPreImaging.all
      def tag(a: MosPreImaging) = a.tag
    }

}