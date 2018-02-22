// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.data.Nested
import cats.implicits._
import gem.enum.{ Instrument, GmosNorthStageMode }
import gem.config.{ DynamicConfig, StaticConfig }
import gem.math.{ Coordinates, ProperMotion }

// An example of how to deal with an instrument-indexed observation and dispatch from a generic
// observation.
object ObservationExample {

  // An alias for the type of Gmos North observations. Do we want these generally?
  type GmosNorthObservation = Observation.Full.Aux[Instrument.GmosN.type]

  // A method that manipulates and returns GmosN observation.
  def gmosMethod(o: GmosNorthObservation): GmosNorthObservation = {

    // Update the asterism. If we use the wrong instrument we'll get a compile error at the end
    // because it won't be a GmosNorthObservation (nor indeed an Observation.Full).
    val t = Target("foo", Right(ProperMotion.const(Coordinates.Zero)))
    val a = Asterism.SingleTarget(t, Instrument.GmosN) // <-- note instrument
    val teʹ = TargetEnvironment.Aux(Some(a), o.targets.userTargets)

    // Update the static config. We must use pattern-matching to inspect it, but the only pattern
    // that will match (or indeed compile) is StaticConfig.GmosNorth.
    val StaticConfig.GmosNorth(c, _) = o.staticConfig
    val scʹ = StaticConfig.GmosNorth(c, GmosNorthStageMode.FollowZ)

    // Update the dynamic config. Again we have to pattern-match but only one case will compile
    // and the match for config patterns is exhaustive. If we return another instrument's
    // DynamicConfig we'll get a compile error below.
    val stepsʹ = Nested(o.steps).map {
      case DynamicConfig.GmosNorth(c, g, f, _) => DynamicConfig.GmosNorth(c, g, f, None)
    } .value // Nested(a).map(f).value = a.map(_.map(f))

    // Update the observation. If any part has an instrument other than GmosN it won't compile.
    o.copy(title = "blah", targets = teʹ, staticConfig = scʹ, steps = stepsʹ)

  }

  // A method that dispatches based on the instrument
  def foo(o: Observation.Full): Observation.Full =
    o.narrow match {
      case (Instrument.GmosN, o) => gmosMethod(o)
      // and so on
    }

}