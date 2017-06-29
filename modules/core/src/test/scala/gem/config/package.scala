// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

import scalaz._
import Scalaz._

package object config {

  // Surely defined somewhere, but I can't find it.
  implicit def arbScalazEither[A, B](implicit aa: Arbitrary[A], ab: Arbitrary[B]): Arbitrary[A \/ B] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[A].map(_.left[B] ),
        arbitrary[B].map(_.right[A])
      )
    }
}
