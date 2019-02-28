// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.{ Order, Show }
import cats.instances.int._
import gem.optics.Format
import gem.syntax.prism._
import monocle.Prism

/**
 * Exact wavelengths represented as unsigned integral picometers in the range [0 .. Int.MaxValue]
 * which means the largest representable wavelength is 2.147483647 mm.
 * @param toPicometers This wavelength in integral picometers (10^-12 of a meter).
 */
sealed abstract case class Wavelength private (toPicometers: Int) {
  // Sanity check … should be correct via the companion constructor.
  assert(toPicometers >= 0, s"Invariant violated. $toPicometers is negative.")
}

object Wavelength {

  final lazy val Min: Wavelength = fromPicometers.unsafeGet(0)
  final lazy val Max: Wavelength = fromPicometers.unsafeGet(Int.MaxValue)

  /** @group Typeclass Instances */
  implicit val WavelengthShow: Show[Wavelength] =
    Show.fromToString

  /** @group Typeclass Instances */
  implicit val WavelengthOrd: Order[Wavelength] =
    Order.by(_.toPicometers)

  /**
   * Prism from Int in pm into Wavelength and back.
   * @group Optics
   */
  def fromPicometers: Prism[Int, Wavelength] =
    Prism((n: Int) => Some(n).filter(_ >= 0).map(new Wavelength(_) {}))(_.toPicometers)

  /**
   * Prism from Int in Å into Wavelength and back.
   * @group Optics
   */
  def fromAngstroms: Format[Int, Wavelength] =
    fromPicometers.asFormat.imapA(_ / 100, _ * 100)

  /**
   * Prism from Int in nm into Wavelength and back.
   * @group Optics
   */
  def fromNanometers: Format[Int, Wavelength] =
    fromPicometers.asFormat.imapA(_ / 1000, _ * 1000)

  /**
   * Prism from Int in μm into Wavelength and back.
   * @group Optics
   */
  def fromMicrometers: Format[Int, Wavelength] =
    fromPicometers.asFormat.imapA(_ / 1000000, _ * 1000000)

}
