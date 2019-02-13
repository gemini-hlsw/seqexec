// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.arb

import gem.util.Zipper

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Cogen}


trait ArbZipper {

  implicit def arbZipper[A: Arbitrary]: Arbitrary[Zipper[A]] =
    Arbitrary {
      for {
        l <- arbitrary[List[A]]
        f <- arbitrary[A]
        r <- arbitrary[List[A]]
      } yield Zipper(l, f, r)
    }

  implicit def cogZipper[A: Cogen]: Cogen[Zipper[A]] =
    Cogen[List[A]].contramap(_.toList)

}

object ArbZipper extends ArbZipper
