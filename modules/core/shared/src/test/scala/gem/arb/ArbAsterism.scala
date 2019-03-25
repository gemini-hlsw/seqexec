// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.enum.Instrument

import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._

trait ArbAsterism {

  import ArbEnumerated._
  import ArbTarget._

  def genSingleTarget(i: Instrument): Gen[Asterism] =
    arbitrary[Target].map(Asterism.unsafeFromSingleTarget(_, i))

  val genGhostDualTarget: Gen[Asterism.GhostDualTarget] =
    for {
      t1 <- arbitrary[Target]
      t2 <- arbitrary[Target]
    } yield Asterism.GhostDualTarget(t1, t2)

  def genAsterism(i: Instrument): Gen[Asterism] =
    i match {
      case Instrument.Ghost => genGhostDualTarget
      case _                => genSingleTarget(i)
    }

  implicit val arbAsterism: Arbitrary[Asterism] =
    Arbitrary {
      for {
        i <- arbitrary[Instrument]
        a <- genAsterism(i)
      } yield a
    }

  implicit def cogAsterism: Cogen[Asterism] =
    Cogen[(List[Target], String)].contramap(a => (a.targets.toList, a.productPrefix))

}

object ArbAsterism extends ArbAsterism
