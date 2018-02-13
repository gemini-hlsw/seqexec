// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.math.Wavelength
import gem.util.Enumerated

/**
 * Enumerated type for GNRIS Wavelength suggestion.
 * @group Enumerations (Generated)
 */
sealed abstract class GnirsWaveLengthSuggestion(
  val tag: String,
  val shortName: String,
  val longName: String,
  val wavelength: Wavelength
) extends Product with Serializable {
  type Self = this.type
}

object GnirsWaveLengthSuggestion {

  type Aux[A] = GnirsWaveLengthSuggestion { type Self = A }

  /** @group Constructors */ case object One extends GnirsWaveLengthSuggestion("One", "4.85", "4.85", Wavelength.fromAngstroms.unsafeGet(48500))
  /** @group Constructors */ case object Two extends GnirsWaveLengthSuggestion("Two", "3.40", "3.40", Wavelength.fromAngstroms.unsafeGet(34000))
  /** @group Constructors */ case object Three extends GnirsWaveLengthSuggestion("Three", "2.22", "2.22", Wavelength.fromAngstroms.unsafeGet(22200))
  /** @group Constructors */ case object Four extends GnirsWaveLengthSuggestion("Four", "1.65", "1.65", Wavelength.fromAngstroms.unsafeGet(16500))
  /** @group Constructors */ case object Five extends GnirsWaveLengthSuggestion("Five", "1.63", "1.63", Wavelength.fromAngstroms.unsafeGet(16300))
  /** @group Constructors */ case object Six extends GnirsWaveLengthSuggestion("Six", "1.25", "1.25", Wavelength.fromAngstroms.unsafeGet(12500))
  /** @group Constructors */ case object Seven extends GnirsWaveLengthSuggestion("Seven", "1.10", "1.10", Wavelength.fromAngstroms.unsafeGet(11000))

  /** All members of GnirsWaveLengthSuggestion, in canonical order. */
  val all: List[GnirsWaveLengthSuggestion] =
    List(One, Two, Three, Four, Five, Six, Seven)

  /** Select the member of GnirsWaveLengthSuggestion with the given tag, if any. */
  def fromTag(s: String): Option[GnirsWaveLengthSuggestion] =
    all.find(_.tag === s)

  /** Select the member of GnirsWaveLengthSuggestion with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): GnirsWaveLengthSuggestion =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val GnirsWaveLengthSuggestionEnumerated: Enumerated[GnirsWaveLengthSuggestion] =
    new Enumerated[GnirsWaveLengthSuggestion] {
      def all = GnirsWaveLengthSuggestion.all
      def tag(a: GnirsWaveLengthSuggestion) = a.tag
    }

}