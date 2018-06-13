// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.{ Order, Show }
import cats.instances.int._
import gem.syntax.prism._
import monocle.Prism

/**
 * Exact wavelengths represented as unsigned integral angstroms in the range [0 .. Int.MaxValue]
 * which means the largest representable wavelength is 214.7483647 mm.
 * @param toAngstroms This wavelength in integral angstroms (10^-10 of a meter).
 */
sealed abstract case class Wavelength private (toAngstroms: Int) {
  // Sanity check … should be correct via the companion constructor.
  assert(toAngstroms >= 0, s"Invariant violated. $toAngstroms is negative.")
}

object Wavelength {

  final lazy val Min: Wavelength = fromAngstroms.unsafeGet(0)
  final lazy val Max: Wavelength = fromAngstroms.unsafeGet(Int.MaxValue)

  /** @group Typeclass Instances */
  implicit val WavelengthShow: Show[Wavelength] =
    Show.fromToString

  /** @group Typeclass Instances */
  implicit val WavelengthOrd: Order[Wavelength] =
    Order.by(_.toAngstroms)

  /**
   * Prism from Int into Wavelength and back.
   * @group Optics
   */
  def fromAngstroms: Prism[Int, Wavelength] =
    Prism((n: Int) => Some(n).filter(_ >= 0).map(new Wavelength(_) {}))(_.toAngstroms)

}
