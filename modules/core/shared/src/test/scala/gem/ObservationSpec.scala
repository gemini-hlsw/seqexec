// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

// import cats.{ Eq, Show }
// import cats.kernel.laws.discipline._
import cats.tests.CatsSuite
// import gem.arb._
import gem.enum.Instrument

final class ObservationSpec extends CatsSuite {
  // import ArbObservation._

  // We don't need to call this method; the fact that it compiles proves that the type equalities
  // hold for any Observation.Full.Aux.
  def verifyTypeEqualities[I <: Instrument with Singleton](o: Observation.Full.Aux[I]): Any = {
    val te = o.targets
    val sc = o.staticConfig
    val dc = o.steps(0).dynamicConfig
    (
      implicitly[te.I =:= sc.I],
      implicitly[te.I =:= dc.I],
      implicitly[sc.I =:= te.I],
      implicitly[sc.I =:= dc.I],
      implicitly[dc.I =:= te.I],
      implicitly[dc.I =:= sc.I],
    )
  }

  // We don't need to call this method either; the fact that it compiles proves that the type
  // equalities hold for any Observation.Full.
  def verifyTypeEqualitiesʹ(o: Observation.Full): Any =
   verifyTypeEqualities(o)



}
