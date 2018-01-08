// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import org.scalacheck._

package object arb {

  implicit class MoreGenOps[A](g: Gen[A]) {

    /** Like `retryUntil` but retries until the specified PartialFunction is defined. */
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    def collectUntil[B](f: PartialFunction[A, B]): Gen[B] =
      g.map(a => f.lift(a)).retryUntil(_.isDefined).map(_.get)

    /** Branch randomly. */
    def flatMapOneOf[B](f: A => Gen[B], fs: (A => Gen[B])*): Gen[B] =
      Gen.oneOf(f +: fs).flatMap(g.flatMap)

  }

  // This isn't in scalacheck for whatever reason
  implicit def mapCogen[A: Cogen, B: Cogen]: Cogen[Map[A, B]] =
    Cogen[List[(A, B)]].contramap(_.toList)

}
