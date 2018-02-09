// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.enum.Instrument
import gem.syntax.treesetcompanion._

import org.scalacheck._
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._
import scala.collection.immutable.TreeSet
import shapeless.Witness

trait ArbTargetEnvironment {
  import ArbAsterism._
  import ArbEnumerated._
  import ArbUserTarget._

  implicit def arbTargetEnvironment[I <: Instrument with Singleton](
    implicit w: Witness.Aux[I]
  ): Arbitrary[TargetEnvironment.Aux[I]] =
    Arbitrary { genTargetEnvironment(w.value) }

  def genTargetEnvironment[I <: Instrument with Singleton](i: I): Gen[TargetEnvironment.Aux[I]] =
    for {
      a <- frequency((9, genAsterism(i).map(Option(_))), (1, const(Option.empty[Asterism.Aux[I]])))
      n <- choose(0, 10)
      u <- listOfN(n, arbitrary[UserTarget]).map(us => TreeSet.fromList(us))
    } yield TargetEnvironment.Aux(a, u)

  implicit def cogTargetEnvironment[I <: Instrument with Singleton]: Cogen[TargetEnvironment.Aux[I]] =
    Cogen[(Option[Asterism.Aux[I]], List[UserTarget])].contramap(e => (e.asterism, e.userTargets.toList))

}

object ArbTargetEnvironment extends ArbTargetEnvironment