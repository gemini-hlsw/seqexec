// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.math

import cats.{ Order, Show }
import cats.instances.int._
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


  // A Format such as this one which converts to Å:
  //  fromPicometers.asFormat.imapA(_ / 100, _ * 100)
  // is not a valid Format since λ => Int (Å) => λ loses precision

  /**
   * Poor man's uncomposable "optics" for converting between wavelength units.
   * @param name
   * @param exp power of 10 difference relative to pm (Å is 2, nm is 3, μm 6)
   */
  sealed abstract case class UnitConverter(name: String, exp: Int) {
    private val p: Int = BigInt(10).pow(exp).toInt

    /**
     * Max value in these units that can be stored in a Wavelength.
     */
    val maxValue: Int =
      Int.MaxValue / p

    /**
     * Creates a Wavelength from an Int in the corresponding units, provided it
     * is in the range 0 to `maxValue`.
     * @param n value in corresponding unit
     * @return Some(Wavelength) provided n is in range [0, `maxValue`], None
     *         otherwise
     */
    def getOption(n: Int): Option[Wavelength] =
      if ((n < 0) || (n > maxValue)) None else fromPicometers.getOption(n * p)

    def unsafeGet(n: Int): Wavelength =
      getOption(n).getOrElse(sys.error(s"$n ($name) is not in range [0, $maxValue]"))

    /**
     * Converts Wavelength to an Int in the corresponding units, losing precision.
     */
    def reverseGet(w: Wavelength): Int =
      w.toPicometers / p

  }

  /**
   * Creates a Wavelength from an Int in Å.
   * @param Å in the range (0 to 21474836)
   * @return a Wavelength provided Å is in range
   */
  def fromAngstroms: UnitConverter =
    new UnitConverter("Å", 2) {}

  /**
   * Creates a Wavelength from an Int in nm.
   * @param nm in the range (0 to 2147483)
   * @return a Wavelength provided nm is in range
   */
  def fromNanometers: UnitConverter =
    new UnitConverter("nm", 3) {}

  /**
   * Creates a Wavelength from an Int in μm.
   * @param μm in the range (0 to 2147)
   * @return a Wavelength provided μm is in range
   */
  def fromMicrometers: UnitConverter =
    new UnitConverter("μm", 6) {}

}
