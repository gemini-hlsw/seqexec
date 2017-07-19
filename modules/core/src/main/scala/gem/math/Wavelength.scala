// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import scalaz.{ Order, Show }
import scalaz.std.anyVal.intInstance
import scalaz.syntax.equal._

/**
 * Exact wavelengths represented as unsigned integral Angstroms in the range [0 .. Int.MaxValue]
 * which means the largest representable wavelength is 214.7483647 mm.
 * @param toAngstroms This wavelengths in integral angstroms (10^-10).
 */
sealed class Wavelength private (val toAngstroms: Int) {

  // Sanity checks … should be correct via the companion constructor.
  assert(toAngstroms >= 0, s"Invariant violated. $toAngstroms is negative.")
  assert(toAngstroms <= Int.MaxValue, s"Invariant violated. $toAngstroms is larger than Int.MaxValue.")

  /** String representation of this Wavelength, for debugging purposes only. */
  override def toString =
    f"Wavelength($toAngstroms Å)"

  /** Angles are equal if their magnitudes are equal. */
  override final def equals(a: Any) =
    a match {
      case a: Wavelength => a.toAngstroms === toAngstroms
      case _        => false
    }

  override final def hashCode =
    toAngstroms.toInt

}

object Wavelength {

  final lazy val ZeroAngstroms = unsafeFromAngstroms(0)

  /** Construct a wavelength from integral angstroms, if non-negative. */
  def fromAngstroms(angstroms: Int): Option[Wavelength] =
    Some(angstroms).filter(_ > 0).map(new Wavelength(_))

  /** Construct a wavelength from integral angstroms, raising an exception if negative. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromAngstroms(angstroms: Int): Wavelength =
    fromAngstroms(angstroms).getOrElse(throw new ArithmeticException(s"Wavelength: overflow: $angstroms"))

  /** @group Typeclass Instances */
  implicit val WavelengthShow: Show[Wavelength] =
    Show.showA

  /** @group Typeclass Instances */
  implicit val WavelengthOrd: Order[Wavelength] =
    Order.orderBy(_.toAngstroms)

}
