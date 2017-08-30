// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import EphemerisKey._

import org.scalacheck._
import org.scalacheck.Arbitrary._

trait ArbEphemerisKey {
  private def genStringDes[A](f: String => A): Gen[A] =
    arbitrary[String].map(s => f(s.take(10)))

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
    Cogen[String].contramap(_.format)

}

object ArbEphemerisKey extends ArbEphemerisKey
