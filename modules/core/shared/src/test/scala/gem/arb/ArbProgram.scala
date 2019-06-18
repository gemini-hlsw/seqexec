// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.{ Observation, Program }
import gsp.math.Index
import gsp.math.arb.ArbIndex
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import scala.collection.immutable.SortedMap

trait ArbProgram {
  import ArbIndex._
  import ArbObservation._
  import ArbProgramId._

  implicit val arbProgram: Arbitrary[Program] =
    Arbitrary {
      for {
        id    <- arbitrary[ProgramId]
        title <- arbitrary[String]
        obs   <- arbitrary[SortedMap[Index, Observation]]
      } yield Program(id, title, obs)
    }

}
object ArbProgram extends ArbProgram
