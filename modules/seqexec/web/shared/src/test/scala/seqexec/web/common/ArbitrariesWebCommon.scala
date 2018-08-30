// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.common

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Cogen, Gen}
import cats.data.NonEmptyList

object ArbitrariesWebCommon {

  implicit def arbFixedLengthBuffer[A: Arbitrary]: Arbitrary[FixedLengthBuffer[A]] =
    Arbitrary {
      val maxSize = 100
      for {
        l <- Gen.choose(1, maxSize)
        s <- Gen.choose(0, l - 1)
        d <- Gen.listOfN(s, arbitrary[A])
      } yield FixedLengthBuffer.unsafeFromInt(l, d: _*)
    }

  implicit def fixedLengthBufferCogen[A: Cogen]: Cogen[FixedLengthBuffer[A]] =
    Cogen[(Int, Vector[A])].contramap(x => (x.maxLength, x.toVector))

  implicit def arbZipper[A: Arbitrary]: Arbitrary[Zipper[A]] =
    Arbitrary {
      val maxSize = 100
      for {
        h <- arbitrary[A]
        l <- Gen.choose(1, maxSize)
        d <- Gen.listOfN(l, arbitrary[A])
      } yield Zipper.fromNel(NonEmptyList.of(h, d: _*))
    }

  implicit def zipperCogen[A: Cogen]: Cogen[Zipper[A]] =
    Cogen[List[A]].contramap(_.toList)

}
