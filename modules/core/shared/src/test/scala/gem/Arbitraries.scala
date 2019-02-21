// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

/*
import cats.implicits._
import gem.arb._
import gem.enum.Instrument
import gem.math.Index
import gem.syntax.all._
import org.scalacheck._
import org.scalacheck.Arbitrary._

import scala.collection.immutable.TreeMap
*/
trait Arbitraries {

  /*
  import ArbEnumerated._
  import ArbStaticConfig._
  import ArbStep._
  import ArbTargetEnvironment._

  implicit val arbObservation: Arbitrary[Observation] =
    Arbitrary {
      for {
        i <- Gen.oneOf(
               Instrument.Flamingos2,
               Instrument.GmosN,
               Instrument.GmosS,
             ) // Add more as they become available
        o <- genObservationOf(i)
      } yield o
    }

*/
}
