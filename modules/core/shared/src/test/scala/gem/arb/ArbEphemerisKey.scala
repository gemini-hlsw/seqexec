// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import EphemerisKey._
import gem.enum.EphemerisKeyType

import org.scalacheck._
import org.scalacheck.Arbitrary._

trait ArbEphemerisKey {
  import ArbEnumerated._

  private def genStringDes[A](f: String => A): Gen[A] =
    Gen.alphaNumStr.map(s => f(s.take(10)))

  private def genIntDes[A](f: Int => A): Gen[A] =
    arbitrary[Int].map(f)

  implicit val arbEphemerisKey: Arbitrary[EphemerisKey] =
    Arbitrary {
      Gen.oneOf[EphemerisKey](
        genStringDes(Comet.apply       ),
        genStringDes(AsteroidNew.apply ),
        genIntDes   (AsteroidOld.apply ),
        genIntDes   (MajorBody.apply   ),
        genIntDes   (UserSupplied.apply)
      )
    }

  implicit val CogenEphemerisKey: Cogen[EphemerisKey] =
    Cogen[String].contramap(EphemerisKey.fromString.reverseGet)

  private val perturbations: List[String => Gen[String]] =
    List(
      _ => arbitrary[String],             // swap for a random string
      s => Gen.const(s.replace("2", "0")) // create a leading zero, perhaps
    )

  // Key and des pairs that are often parsable
  val keyAndDes: Gen[(EphemerisKeyType, String)] =
    for {
      k <- arbitrary[EphemerisKeyType]
      d <- arbitrary[Int].map(_.abs.toString).flatMapOneOf(Gen.const, perturbations: _*)
    } yield (k, d)

  // Strings that are often parsable
  val strings: Gen[String] =
    arbitrary[EphemerisKey]
      .map(EphemerisKey.fromString.reverseGet)
      .flatMapOneOf(Gen.const, perturbations: _*)

}

object ArbEphemerisKey extends ArbEphemerisKey
