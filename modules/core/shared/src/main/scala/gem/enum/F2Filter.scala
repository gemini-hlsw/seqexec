// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.syntax.eq._
import cats.instances.string._
import gem.util.Enumerated

/**
 * Enumerated type for Flamingos2 filters.
 * @group Enumerations (Generated)
 */
sealed abstract class F2Filter(
  val tag: String,
  val shortName: String,
  val longName: String,
  val wavelength: Option[gem.math.Wavelength],
  val obsolete: Boolean
) extends Product with Serializable

object F2Filter {

  /** @group Constructors */ case object Y extends F2Filter("Y", "Y", "Y (1.02 um)", Some(gem.math.Wavelength.unsafeFromAngstroms(10200)), false)
  /** @group Constructors */ case object F1056 extends F2Filter("F1056", "F1056", "F1056 (1.056 um)", Some(gem.math.Wavelength.unsafeFromAngstroms(10560)), false)
  /** @group Constructors */ case object J extends F2Filter("J", "J", "J (1.25 um)", Some(gem.math.Wavelength.unsafeFromAngstroms(12500)), false)
  /** @group Constructors */ case object H extends F2Filter("H", "H", "H (1.65 um)", Some(gem.math.Wavelength.unsafeFromAngstroms(16500)), false)
  /** @group Constructors */ case object JH extends F2Filter("JH", "JH", "JH (spectroscopic)", Some(gem.math.Wavelength.unsafeFromAngstroms(13900)), false)
  /** @group Constructors */ case object HK extends F2Filter("HK", "HK", "HK (spectroscopic)", Some(gem.math.Wavelength.unsafeFromAngstroms(18710)), false)
  /** @group Constructors */ case object JLow extends F2Filter("JLow", "J-low", "J-low (1.15 um)", Some(gem.math.Wavelength.unsafeFromAngstroms(11500)), false)
  /** @group Constructors */ case object KLong extends F2Filter("KLong", "K-long", "K-long (2.20 um)", Some(gem.math.Wavelength.unsafeFromAngstroms(22000)), false)
  /** @group Constructors */ case object KShort extends F2Filter("KShort", "K-short", "K-short (2.15 um)", Some(gem.math.Wavelength.unsafeFromAngstroms(21500)), false)
  /** @group Constructors */ case object F1063 extends F2Filter("F1063", "F1063", "F1063 (1.063 um)", Some(gem.math.Wavelength.unsafeFromAngstroms(10630)), false)
  /** @group Constructors */ case object KBlue extends F2Filter("KBlue", "K-blue", "K-blue (2.06 um)", Some(gem.math.Wavelength.unsafeFromAngstroms(20600)), false)
  /** @group Constructors */ case object KRed extends F2Filter("KRed", "K-red", "K-red (2.31 um)", Some(gem.math.Wavelength.unsafeFromAngstroms(23100)), false)
  /** @group Constructors */ case object Open extends F2Filter("Open", "Open", "Open", Some(gem.math.Wavelength.unsafeFromAngstroms(16000)), true)
  /** @group Constructors */ case object Dark extends F2Filter("Dark", "Dark", "Dark", Option.empty[gem.math.Wavelength], true)

  /** All members of F2Filter, in canonical order. */
  val all: List[F2Filter] =
    List(Y, F1056, J, H, JH, HK, JLow, KLong, KShort, F1063, KBlue, KRed, Open, Dark)

  /** Select the member of F2Filter with the given tag, if any. */
  def fromTag(s: String): Option[F2Filter] =
    all.find(_.tag === s)

  /** Select the member of F2Filter with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): F2Filter =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val F2FilterEnumerated: Enumerated[F2Filter] =
    new Enumerated[F2Filter] {
      def all = F2Filter.all
      def tag(a: F2Filter) = a.tag
    }

}